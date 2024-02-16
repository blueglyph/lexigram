// Copyright 2023 Redglyph
//

use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};

#[derive(PartialEq, Debug)]
pub struct VecTree<T> {
    nodes: Vec<Node<T>>
}

#[derive(PartialEq, Debug)]
pub struct Node<T> {
    data: T,
    children: Vec<usize>
}

#[derive(Clone, Copy)]
enum VisitNode<T> {
    Down(T),
    Up(T)
}

// ---------------------------------------------------------------------------------------------

impl<T> VecTree<T> {
    pub fn new() -> Self {
        VecTree { nodes: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        VecTree { nodes: Vec::with_capacity(capacity) }
    }

    pub fn set_root(&mut self, item: T) -> Result<usize, ()> {
        if self.nodes.is_empty() {
            let node = Node { data: item, children: Vec::new() };
            self.nodes.push(node);
            Ok(0)
        } else {
            Err(())
        }
    }

    pub fn add(&mut self, parent_index: usize, item: T) -> usize {
        let index = self.nodes.len();
        self.nodes[parent_index].children.push(index);
        let node = Node::new(item);
        self.nodes.push(node);
        index
    }

    // pub fn concat<T>(items: T) -> ReNodeId where T: IntoIterator<Item = ReNodeId> {
    //         ReNodeId::new(ReNode::Concat(items.into_iter().collect()))
    //     }

    pub fn add_iter<U: IntoIterator<Item = T>>(&mut self, parent_index: usize, items: U) -> &[usize] {
        for item in items {
            self.add(parent_index, item);
        }
        &self.nodes[parent_index].children
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn get(&self, index: usize) -> &T {
        &self.nodes.get(index).unwrap().data
    }

    pub fn get_mut(&mut self, index: usize) -> &mut T {
        &mut self.nodes.get_mut(index).unwrap().data
    }

    pub fn children(&self, index: usize) -> &[usize] {
        &self.nodes.get(index).unwrap().children
    }

    pub fn iter_children(&self, index: usize) -> impl Iterator<Item = &Node<T>> {
        self.nodes.get(index).unwrap().children.iter().map(|&i| self.nodes.get(i).unwrap())
    }
}

impl<T> Node<T> {
    pub fn new(data: T) -> Self {
        Node { data, children: Vec::new() }
    }

    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }

    pub fn children(&self) -> &[usize] {
        &self.children
    }
}

impl<T> Index<usize> for VecTree<T> {
    type Output = Node<T>;

    fn index(&self, index: usize) -> &Self::Output {
        self.nodes.get(index).unwrap()
    }
}

impl<T> IndexMut<usize> for VecTree<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.nodes.get_mut(index).unwrap()
    }
}

impl<T: Display> Display for VisitNode<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VisitNode::Down(v) => write!(f, "D({v})"),
            VisitNode::Up(v) => write!(f, "U({v})"),
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Common to mutable and immutable iterators

/// Post-order, depth-first search iterator type for `TreeRef` = `&VecTree<T>` or `&mut VecTree<T>`
pub struct VecTreeIter<TreeRef> {
    stack: Vec<VisitNode<usize>>,
    next: Option<VisitNode<usize>>,
    tree: TreeRef
}

impl<TreeRef> VecTreeIter<TreeRef> {
    pub fn new(tree: TreeRef) -> Self {
        VecTreeIter { stack: Vec::new(), next: Some(VisitNode::Down(0)), tree }
    }
}

/// Iterator return type for `TRef` = `&T` or `&mut T`
pub struct NodeProxy<T, TRef> {
    pub index: usize,
    pub data: TRef,
    tree_node_ptr: *const Node<T>,
    tree_size: usize
}

impl<T, TRef> NodeProxy<T, TRef> {
    pub fn iter_children(&self) -> impl DoubleEndedIterator<Item = NodeProxy<T, &T>> {
        assert!(self.index < self.tree_size);
        let children = unsafe { &(*self.tree_node_ptr.add(self.index)).children };
        children.iter().map(|&index| NodeProxy {
            index,
            data: self.get(index),
            tree_node_ptr: self.tree_node_ptr,
            tree_size: self.tree_size
        })
    }

    pub fn iter_children_data(&self) -> impl DoubleEndedIterator<Item = &T> {
        assert!(self.index < self.tree_size);
        let children = unsafe { &(*self.tree_node_ptr.add(self.index)).children };
        children.iter().map(|&c| self.get(c))
    }

    #[inline]
    fn get(&self, index: usize) -> &T {
        assert!(index < self.tree_size);
        unsafe { &(*self.tree_node_ptr.add(index)).data }
    }
}

// ---------------------------------------------------------------------------------------------
// Immutable iterator

impl<'a, T> VecTree<T> {
    pub fn iter_depth(&'a self) -> VecTreeIter<&'a VecTree<T>> {
        VecTreeIter::new(self)
    }
}

// Post-order, depth-first search immutable iterator for `VecTree<T>`
impl<'a, T> Iterator for VecTreeIter<&'a VecTree<T>> {
    type Item = NodeProxy<T, &'a T>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node_dir) = self.next {
            let index_option = match node_dir {
                VisitNode::Down(index) => {
                    let children = unsafe {
                        assert!(index < self.tree.nodes.len());
                        let node = self.tree.nodes.as_ptr().add(index);
                        &node.as_ref().unwrap().children
                    };
                    if children.is_empty() {
                        Some(index.clone())
                    } else {
                        self.stack.push(VisitNode::Up(index.clone()));
                        for index in children.iter().rev() {
                            self.stack.push(VisitNode::Down(*index));
                        }
                        None
                    }
                }
                VisitNode::Up(index) => {
                    Some(index)
                }
            };
            self.next = self.stack.pop();
            if let Some(index) = index_option {
                assert!(index < self.tree.nodes.len());
                return Some(NodeProxy {
                    index,
                    data: unsafe { &(*self.tree.nodes.as_ptr().add(index)).data },
                    tree_node_ptr: self.tree.nodes.as_ptr(),
                    tree_size: self.tree.len(),
                })
            }
        }
        None
    }
}

// ---------------------------------------------------------------------------------------------
// Mutable iterator

impl<'a, T> VecTree<T> {
    pub fn iter_depth_mut(&'a mut self) -> VecTreeIter<&'a mut VecTree<T>> {
        VecTreeIter::new(self)
    }
}

// Post-order, depth-first search mutable iterator for `VecTree<T>`
impl<'a, T> Iterator for VecTreeIter<&'a mut VecTree<T>> {
    type Item = NodeProxy<T, &'a mut T>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node_dir) = self.next {
            let index_option = match node_dir {
                VisitNode::Down(index) => {
                    let children = unsafe {
                        assert!(index < self.tree.nodes.len());
                        let node = self.tree.nodes.as_ptr().add(index);
                        &node.as_ref().unwrap().children
                    };
                    if children.is_empty() {
                        Some(index.clone())
                    } else {
                        self.stack.push(VisitNode::Up(index.clone()));
                        for index in children.iter().rev() {
                            self.stack.push(VisitNode::Down(*index));
                        }
                        None
                    }
                }
                VisitNode::Up(index) => {
                    Some(index)
                }
            };
            self.next = self.stack.pop();
            if let Some(index) = index_option {
                assert!(index < self.tree.nodes.len());
                return Some(NodeProxy {
                    index,
                    data: unsafe { &mut (*self.tree.nodes.as_mut_ptr().add(index)).data },
                    tree_node_ptr: self.tree.nodes.as_ptr(),
                    tree_size: self.tree.len(),
                })
            }
        }
        None
    }
}

// ---------------------------------------------------------------------------------------------

