use std::fmt::Display;
use crate::vectree::VecTree;

#[cfg(test)]
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

#[cfg(test)]
fn tree_to_string<T: Display>(tree: &VecTree<T>) -> String {
    if tree.len() > 0 {
        node_to_string(tree, 0)
    } else {
        "None".to_string()
    }
}

#[cfg(test)]
fn build_tree() -> VecTree<String> {
    let mut tree = VecTree::new();
    let root = tree.set_root("root".to_string()).expect("empty tree");
    let a = tree.add(root, "a".to_string());
    let _ = tree.add(root, "b".to_string());
    let c = tree.add(root, "c".to_string());
    tree.add(a, "a1".to_string());
    tree.add(a, "a2".to_string());
    tree.add(c, "c1".to_string());
    tree.add(c, "c2".to_string());
    tree
}

#[cfg(test)]
mod tests {
    use super::*;

    // NOT NECESSARY: set MIRIFLAGS=-Zmiri-tree-borrows
    // cargo +nightly miri test --lib tree7_vec_mutitem::tests::iter_depth_children_simple -- --exact
    #[test]
    fn iter_depth_children_simple() {
        let tree = build_tree();
        assert_eq!(tree_to_string(&tree), "root(a(a1,a2),b,c(c1,c2))");

        let mut result = String::new();
        for inode in tree.iter_depth() {
            let main_lineage = inode.data.to_lowercase().starts_with('c')
                || inode.iter_children_data().any(|n| n.to_lowercase().starts_with('c'));
            if main_lineage {
                result.push_str(&inode.data.to_uppercase());
            } else {
                result.push_str(&inode.data);
            }
            result.push(',');
        }
        assert_eq!(result, "a1,a2,a,b,C1,C2,C,ROOT,");
    }

    // NOT NECESSARY: set MIRIFLAGS=-Zmiri-tree-borrows
    // cargo +nightly miri test --lib tree7_vec_mutitem::tests::iter_depth_children -- --exact
    #[test]
    fn iter_depth_children() {
        let tree = build_tree();
        let mut result = String::new();
        for inode in tree.iter_depth() {
            // condition: any child j begins with 'c' and has all j's children k begin with 'c'
            let sub_is_c = inode.iter_children()
                .any(|j| {
                    j.data.to_lowercase().starts_with('c') &&
                        j.iter_children_data().all(|k| k.to_lowercase().starts_with('c'))
                });
            if sub_is_c {
                result.push_str(&inode.data.to_uppercase());
            } else {
                result.push_str(&inode.data);
            }
            result.push(',');
        }
        assert_eq!(result, "a1,a2,a,b,c1,c2,C,ROOT,");
    }

    // NOT NECESSARY: set MIRIFLAGS=-Zmiri-tree-borrows
    // cargo +nightly miri test --lib tree7_vec_mutitem::tests::iter_depth_mut_children_simple -- --exact
    #[test]
    fn iter_depth_mut_children_simple() {
        let mut tree = build_tree();
        for inode in tree.iter_depth_mut() {
            let main_lineage = inode.data.to_lowercase().starts_with('c')
                || inode.iter_children_data().any(|n| n.to_lowercase().starts_with('c'));
            if main_lineage {
                *inode.data = inode.data.to_uppercase();
            }
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(a(a1,a2),b,C(C1,C2))");
    }

    // NOT NECESSARY: set MIRIFLAGS=-Zmiri-tree-borrows
    // cargo +nightly miri test --lib tree7_vec_mutitem::tests::iter_depth_mut_children -- --exact
    #[test]
    fn iter_depth_mut_children() {
        let mut tree = build_tree();
        for inode in tree.iter_depth_mut() {
            // condition: any child j begins with 'c' and has all j's children k begin with 'c'
            let sub_is_c = inode.iter_children()
                .any(|j| {
                    j.data.to_lowercase().starts_with('c') &&
                        j.iter_children_data().all(|k| k.to_lowercase().starts_with('c'))
                });
            if sub_is_c {
                *inode.data = inode.data.to_uppercase();
            }
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(a(a1,a2),b,C(c1,c2))");
    }

    // NOT NECESSARY: set MIRIFLAGS=-Zmiri-tree-borrows
    // cargo +nightly miri test --lib tree7_vec_mutitem::tests::iter_depth_mut_children_miri -- --exact
    #[test]
    fn iter_depth_mut_children_miri() {
        let mut tree = build_tree();
        let inodes = tree.iter_depth_mut().collect::<Vec<_>>();
        for inode in inodes {
            // condition: any child j begins with 'c' and has all j's children k begin with 'c'
            let sub_is_c = inode.iter_children()
                .any(|j| {
                    j.data.to_lowercase().starts_with('c') &&
                        j.iter_children_data().all(|k| k.to_lowercase().starts_with('c'))
                });
            if sub_is_c {
                *inode.data = inode.data.to_uppercase();
            }
        }
        let result = tree_to_string(&tree);
        assert_eq!(result, "ROOT(a(a1,a2),b,C(c1,c2))");
    }

    #[test]
    fn node_proxy_double_ended() {
        let tree = build_tree();
        let mut result1 = Vec::new();
        let mut result2 = Vec::new();
        for inode in tree.iter_depth() {
            result1.push(format!("{}:{}", inode.data, &inode.iter_children_data().rev().map(|s| s.to_string()).collect::<Vec<_>>().join(",")));
            result2.push(format!("{}:{}", inode.data, &inode.iter_children().rev().map(|s| s.data.to_string()).collect::<Vec<_>>().join(",")));
        }
        assert_eq!(result1, vec!["a1:", "a2:", "a:a2,a1", "b:", "c1:", "c2:", "c:c2,c1", "root:c,b,a"]);
        assert_eq!(result1, result2);
    }
}
