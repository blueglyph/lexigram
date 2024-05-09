#![cfg(test)]

use std::fmt::Display;
use crate::vectree::VecTree;

// ---------------------------------------------------------------------------------------------
// Supporting functions

fn node_to_string<T: Display>(tree: &VecTree<T>, index: usize) -> String {
    let mut result = tree.get(index).to_string();
    let children = tree.children(index);
    if !children.is_empty() {
        result.push_str("(");
        result.push_str(&children.iter().map(|&c| node_to_string(&tree, c)).collect::<Vec<_>>().join(","));
        result.push_str(")");
    }
    result
}

pub(crate) fn tree_to_string<T: Display>(tree: &VecTree<T>) -> String {
    if let Some(id) = tree.root {
        node_to_string(tree, id)
    } else {
        "None".to_string()
    }
}

fn build_tree() -> VecTree<String> {
    let mut tree = VecTree::new();
    let root = tree.add_root("root".to_string());
    let a = tree.add(Some(root), "a".to_string());
    let _ = tree.add(Some(root), "b".to_string());
    let c = tree.add(Some(root), "c".to_string());
    tree.add_iter(Some(a), ["a1".to_string(), "a2".to_string()]);
    tree.add_iter(Some(c), ["c1", "c2"].map(|s| s.to_string()));
    tree
}

// ---------------------------------------------------------------------------------------------
// Tests

mod general {
    use super::*;

    #[test]
    fn test_build_tree() {
        let tree = build_tree();
        assert_eq!(tree_to_string(&tree), "root(a(a1,a2),b,c(c1,c2))");
    }

    #[test]
    fn clear() {
        let mut tree = build_tree();
        tree.clear();
        assert_eq!(tree.nodes.len(), 0);
        assert_eq!(tree.borrows.get(), 0);
    }

    // cargo +nightly miri test --lib vectree::tests::general::clone -- --exact
    #[test]
    fn clone() {
        let tree = build_tree();
        let other_tree = tree.clone();
        drop(tree);
        assert_eq!(tree_to_string(&other_tree), "root(a(a1,a2),b,c(c1,c2))");
    }

    // cargo +nightly miri test --lib vectree::tests::general::iter_depth_children_simple -- --exact
    #[test]
    fn iter_depth_simple() {
        let tree = build_tree();
        let mut result = String::new();
        let mut result_index = vec![];
        for inode in tree.iter_depth_simple() {
            result.push_str(&inode.to_uppercase());
            result.push(',');
            result_index.push(inode.index);
        }
        assert_eq!(result, "A1,A2,A,B,C1,C2,C,ROOT,");
        assert_eq!(result_index, [4, 5, 1, 2, 6, 7, 3, 0]);
    }

    #[test]
    fn iter_depth_simple_at() {
        let tree = build_tree();
        let mut result = String::new();
        let mut result_index = vec![];
        for inode in tree.iter_depth_simple_at(3) {
            result.push_str(&inode);
            result.push(',');
            result_index.push(inode.index);
        }
        assert_eq!(result, "c1,c2,c,");
        assert_eq!(result_index, [6, 7, 3]);
    }

    // cargo +nightly miri test --lib vectree::tests::general::iter_depth -- --exact
    #[test]
    fn iter_depth() {
        let tree = build_tree();
        let mut result = String::new();
        let mut result_index = vec![];
        let mut result_num_children = vec![];
        let mut result_size_subtree = vec![];
        for inode in tree.iter_depth() {
            let main_lineage = inode.to_lowercase().starts_with('c')
                || inode.iter_children().any(|n| n.to_lowercase().starts_with('c'));
            let main_lineage_simple = inode.to_lowercase().starts_with('c')
                || inode.iter_children_simple().any(|n| n.to_lowercase().starts_with('c'));
            assert_eq!(main_lineage, main_lineage_simple);
            if main_lineage {
                result.push_str(&inode.to_uppercase());
            } else {
                result.push_str(&inode);
            }
            result.push(',');
            result_index.push(inode.index);
            result_num_children.push(inode.num_children());
            let mut n = 0;
            for _ichild in inode.iter_depth_simple() {
                n += 1;
            }
            result_size_subtree.push(n);
        }
        assert_eq!(result, "a1,a2,a,b,C1,C2,C,ROOT,");
        assert_eq!(result_index, [4, 5, 1, 2, 6, 7, 3, 0]);
        assert_eq!(result_num_children, [0, 0, 2, 0, 0, 0, 2, 3]);
        assert_eq!(result_size_subtree, [1, 1, 3, 1, 1, 1, 3, 8]);
    }

    #[test]
    fn iter_depth_at() {
        let tree = build_tree();
        let mut result = String::new();
        let mut result_index = vec![];
        for inode in tree.iter_depth_at(3) {
            result.push_str(&inode);
            result.push(',');
            result_index.push(inode.index);
        }
        assert_eq!(result, "c1,c2,c,");
        assert_eq!(result_index, [6, 7, 3]);
    }

    #[test]
    fn add_from_iter() {
        let mut tree = build_tree();
        let other = tree.clone();
        tree.add_from_tree(Some(6), other.iter_depth());
        assert_eq!(tree_to_string(&tree), "root(a(a1,a2),b,c(c1(root(a(a1,a2),b,c(c1,c2))),c2))");
        tree.add_from_tree(Some(4), other.iter_depth_at(3));
        assert_eq!(tree_to_string(&tree), "root(a(a1(c(c1,c2)),a2),b,c(c1(root(a(a1,a2),b,c(c1,c2))),c2))");
    }

    // cargo +nightly miri test --lib vectree::tests::general::iter_depth_children -- --exact
    #[test]
    fn iter_depth_children() {
        let tree = build_tree();
        let mut result = String::new();
        for inode in tree.iter_depth() {
            // condition: any child j begins with 'c' and has all j's children k begin with 'c'
            let sub_is_c = inode.iter_children()
                .any(|j| {
                    j.to_lowercase().starts_with('c') &&
                        j.iter_children().all(|k| k.to_lowercase().starts_with('c'))
                });
            if sub_is_c {
                result.push_str(&inode.to_uppercase());
            } else {
                result.push_str(&inode);
            }
            result.push(',');
        }
        assert_eq!(result, "a1,a2,a,b,c1,c2,C,ROOT,");
    }

    // cargo +nightly miri test --lib vectree::tests::general::iter_depth_simple_mut -- --exact
    #[test]
    fn iter_depth_simple_mut() {
        let mut tree = build_tree();
        let mut result_index = vec![];
        for mut inode in tree.iter_depth_simple_mut() {
            *inode = inode.to_uppercase();
            result_index.push(inode.index);
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(A(A1,A2),B,C(C1,C2))");
        assert_eq!(result_index, [4, 5, 1, 2, 6, 7, 3, 0]);
    }

    #[test]
    fn iter_depth_simple_mut_at() {
        let mut tree = build_tree();
        let mut result = String::new();
        let mut result_index = vec![];
        for mut inode in tree.iter_depth_simple_mut_at(3) {
            *inode = inode.to_uppercase();
            result.push_str(&inode);
            result.push(',');
            result_index.push(inode.index);
        }
        assert_eq!(result, "C1,C2,C,");
        assert_eq!(result_index, [6, 7, 3]);
    }

    // cargo +nightly miri test --lib vectree::tests::general::iter_depth_mut -- --exact
    #[test]
    fn iter_depth_mut() {
        let mut tree = build_tree();
        let mut result_index = vec![];
        let mut result_num_children = vec![];
        let mut result_size_subtree = vec![];
        for mut inode in tree.iter_depth_mut() {
            let main_lineage = inode.to_lowercase().starts_with('c')
                || inode.iter_children().any(|n| n.to_lowercase().starts_with('c'));
            let main_lineage_simple = inode.to_lowercase().starts_with('c')
                || inode.iter_children_simple().any(|n| n.to_lowercase().starts_with('c'));
            assert_eq!(main_lineage, main_lineage_simple);
            if main_lineage {
                *inode = inode.to_uppercase();
            }
            result_index.push(inode.index);
            result_num_children.push(inode.num_children());
            let mut n = 0;
            for _ichild in inode.iter_depth_simple() {
                n += 1;
            }
            result_size_subtree.push(n);
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(a(a1,a2),b,C(C1,C2))");
        assert_eq!(result_index, [4, 5, 1, 2, 6, 7, 3, 0]);
        assert_eq!(result_num_children, [0, 0, 2, 0, 0, 0, 2, 3]);
        assert_eq!(result_size_subtree, [1, 1, 3, 1, 1, 1, 3, 8]);
    }

    #[test]
    fn iter_depth_mut_at() {
        let mut tree = build_tree();
        let mut result = String::new();
        let mut result_index = vec![];
        for mut inode in tree.iter_depth_mut_at(3) {
            *inode = inode.to_uppercase();
            result.push_str(&inode);
            result.push(',');
            result_index.push(inode.index);
        }
        assert_eq!(result, "C1,C2,C,");
        assert_eq!(result_index, [6, 7, 3]);
    }

    // cargo +nightly miri test --lib vectree::tests::general::iter_depth_mut_children -- --exact
    #[test]
    fn iter_depth_mut_children() {
        let mut tree = build_tree();
        for mut inode in tree.iter_depth_mut() {
            // condition: any child j begins with 'c' and has all j's children k begin with 'c'
            let sub_is_c = inode.iter_children()
                .any(|j| {
                    j.to_lowercase().starts_with('c') &&
                        j.iter_children().all(|k| k.to_lowercase().starts_with('c'))
                });
            if sub_is_c {
                *inode = inode.to_uppercase();
            }
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(a(a1,a2),b,C(c1,c2))");
    }

    // cargo +nightly miri test --lib vectree::tests::general::iter_depth_mut_children_simple_miri -- --exact
    #[test]
    fn iter_depth_mut_children_simple_miri() {
        let mut tree = build_tree();
        let inodes = tree.iter_depth_simple_mut().collect::<Vec<_>>();
        for mut inode in inodes {
            *inode = inode.to_uppercase();
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(A(A1,A2),B,C(C1,C2))");
    }

    // cargo +nightly miri test --lib vectree::tests::general::iter_depth_mut_children_miri -- --exact
    #[test]
    fn iter_depth_mut_children_miri() {
        let mut tree = build_tree();
        let inodes = tree.iter_depth_mut().collect::<Vec<_>>();
        for mut inode in inodes {
            *inode = inode.to_uppercase();
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(A(A1,A2),B,C(C1,C2))");
    }
}

mod borrow {
    use super::*;

    #[test]
    #[should_panic(expected="pending mutable reference(s) on children")]
    fn iter_depth_mut_children_bad() {
        let mut tree = build_tree();
        let inodes = tree.iter_depth_mut().collect::<Vec<_>>();
        for mut inode in inodes {
            // condition: any child j begins with 'c' and has all j's children k begin with 'c'
            let sub_is_c = inode.iter_children()
                .any(|j| {
                    //----------------------------------------------------------------------
                    // SHOULD PANIC: we want immutable reference to children while there are
                    //               pending mutable references (in inodes):
                    // j.to_lowercase().starts_with('c') &&
                    //     j.iter_children_data().all(|k| k.to_lowercase().starts_with('c'))
                    j.to_lowercase().starts_with('c') &&
                        j.iter_children().all(|k| k.to_lowercase().starts_with('c'))
                    //----------------------------------------------------------------------
                });
            if sub_is_c {
                *inode = inode.to_uppercase();
            }
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(a(a1,a2),b,C(c1,c2))");
    }

    #[test]
    #[should_panic(expected="pending mutable reference(s) on children when requesting immutable references on them")]
    fn iter_depth_mut_borrow() {
        let mut tree = build_tree();
        {
            // a1,a2,a,b,c1,c2,c,root
            let mut inodes = tree.iter_depth_mut();
            let mut a1_write = inodes.next().unwrap();  // taking   a1
            inodes.next();                              // skipping a2
            let a_write = inodes.next().unwrap();       // taking   a
            //----------------------------------------------------------------------
            // SHOULD PANIC: we want immutable reference to children while there are
            //               pending mutable references (a1_write):
            // let a1_read = a_write.iter_children_data().nth(0).unwrap(); // another ref to a1
            let a1_read = a_write.iter_children().nth(0).unwrap(); // another ref to a1
            //----------------------------------------------------------------------
            let a1_a = a1_read.clone();
            *a1_write = "A1".to_string();               // !!
            let a1_b = a1_read.clone();
            assert_eq!(a1_a, "a1");                     // might fail
            assert_eq!(a1_b, "A1");                     // might fail
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "root(a(A1,a2),b,c(c1,c2))");
    }
}

mod alternate_root {
    use super::*;

    fn build_tree2() -> VecTree<String> {
        let mut tree = VecTree::new();
        let a = tree.add(None, "a".to_string());
        let b = tree.add(None, "b".to_string());
        let c = tree.add(None, "c".to_string());
        let root = tree.addci_iter(None, "root".to_string(), [a, b, c]);
        tree.add_iter(Some(a), ["a1".to_string(), "a2".to_string()]);
        tree.add_iter(Some(c), ["c1", "c2"].map(|s| s.to_string()));
        tree.set_root(root);
        tree
    }

    #[test]
    fn test_build_tree2() {
        let tree = build_tree2();
        assert_eq!(tree_to_string(&tree), "root(a(a1,a2),b,c(c1,c2))");
    }

    #[test]
    fn test_iterators() {
        let mut tree = build_tree2();
        let mut result = String::new();
        for i in tree.iter_depth_simple() {
            result.push_str(&format!("{}:{}", i.index, &i.to_string()));
            result.push(',');
        }
        assert_eq!(result, "4:a1,5:a2,0:a,1:b,6:c1,7:c2,2:c,3:root,");
        result.clear();
        for i in tree.iter_depth() {
            result.push_str(&format!("{}:{}", i.index, &i.to_string()));
            if i.num_children() > 0 {
                result.push('(');
                for j in i.iter_children_simple() {
                    result.push_str(j);
                    result.push(',');
                }
                result.push(')');
            }
            result.push(',');
        }
        assert_eq!(result, "4:a1,5:a2,0:a(a1,a2,),1:b,6:c1,7:c2,2:c(c1,c2,),3:root(a,b,c,),");
        for mut i in tree.iter_depth_simple_mut() {
            if i.starts_with("a") {
                *i = i.to_uppercase();
            }
        }
        assert_eq!(tree_to_string(&tree), "root(A(A1,A2),b,c(c1,c2))");
        for mut i in tree.iter_depth_mut() {
            if i.index != 3 && i.num_children() > 0 {
                *i = "-".to_string();
            }
        }
        assert_eq!(tree_to_string(&tree), "root(-(A1,A2),b,-(c1,c2))");
    }

    #[test]
    fn clone() {
        let tree = build_tree();
        let other_tree = tree.clone();
        drop(tree);
        assert_eq!(tree_to_string(&other_tree), "root(a(a1,a2),b,c(c1,c2))");
    }
}

#[allow(unused)]
mod failures {
    /// ```rust,compile_fail
    /// use test_links::tree10_vec_mutitem::VecTree;
    /// let mut tree = VecTree::new();
    /// let a = tree.get_mut(1);
    /// let b = tree.get(2); // cannot borrow `tree` as immutable because it is also borrowed as mutable
    /// *a = "0".to_string();
    /// ```
    fn must_not_compile1() {}

    /// ```rust,compile_fail
    /// use test_links::tree10_vec_mutitem::VecTree;
    /// let mut tree = VecTree::new();
    /// let a = tree.get(1);
    /// let b = tree.get_mut(2); // cannot borrow `tree` as mutable because it is also borrowed as immutable
    /// *b = "0".to_string();
    /// assert_eq!(a, "a");
    /// ```
    fn must_not_compile2() {}

    /// ```rust,compile_fail
    /// use test_links::tree10_vec_mutitem::VecTree;
    /// let mut tree = VecTree::<String>::new();
    /// let a = tree.get(1);
    /// for mut inode in tree.iter_depth_mut() { // cannot borrow `tree` as mutable because it is also borrowed as immutable
    ///     *inode = inode.to_uppercase();
    /// }
    /// assert_eq!(a, "A");
    /// ```
    fn must_not_compile3() {}
}
