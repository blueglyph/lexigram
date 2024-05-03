use std::cell::{Cell, UnsafeCell};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::ptr::NonNull;

mod tests;

#[derive(Debug)]
pub struct VecTree<T> {
    nodes: Vec<Node<T>>,
    borrows: Cell<u32>,
    root: Option<usize>
}

#[derive(Debug)]
pub struct Node<T> {
    data: UnsafeCell<T>,
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
        VecTree { nodes: Vec::new(), borrows: Cell::new(0), root: None }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        VecTree { nodes: Vec::with_capacity(capacity), borrows: Cell::new(0), root: None }
    }

    pub fn set_root(&mut self, index: usize) -> usize {
        assert!(index < self.nodes.len());
        self.root = Some(index);
        index
    }

    pub fn add_root(&mut self, item: T) -> usize {
        self.root = Some(self.add(None, item));
        self.root.unwrap()
    }

    pub fn add(&mut self, parent_index: Option<usize>, item: T) -> usize {
        let index = self.nodes.len();
        if let Some(parent_index) = parent_index {
            self.nodes[parent_index].children.push(index);
        }
        let node = Node { data: UnsafeCell::new(item), children: Vec::new() };
        self.nodes.push(node);
        index
    }

    pub fn addc(&mut self, parent_index: Option<usize>, item: T, child: T) -> usize {
        let index = self.add(parent_index, item);
        self.add(Some(index), child);
        index
    }

    pub fn addi(&mut self, parent_index: Option<usize>, item: T, child_id: usize) -> usize {
        let node_id = self.add(parent_index, item);
        self.nodes[node_id].children.push(child_id);
        node_id
    }

    pub fn addi_iter<U: IntoIterator<Item = usize>>(&mut self, parent_index: Option<usize>, item: T, children_id: U) -> usize {
        let node_id = self.add(parent_index, item);
        self.nodes[node_id].children.extend(children_id);
        node_id
    }

    pub fn add_iter<U: IntoIterator<Item = T>>(&mut self, parent_index: Option<usize>, items: U) -> Vec<usize> {
        let mut indices = Vec::new();
        for item in items {
            indices.push(self.add(parent_index, item));
        }
        indices
    }

    pub fn addc_iter<U: IntoIterator<Item = T>>(&mut self, parent_index: Option<usize>, item: T, children: U) -> usize {
        let index = self.add(parent_index, item);
        self.add_iter(Some(index), children);
        index
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn get(&self, index: usize) -> &T {
        unsafe { &*self.nodes.get(index).unwrap().data.get() }
    }

    pub fn get_mut(&mut self, index: usize) -> &mut T {
        self.nodes.get_mut(index).unwrap().data.get_mut()
    }

    pub fn children(&self, index: usize) -> &[usize] {
        &self.nodes.get(index).unwrap().children
    }

    pub fn iter_children(&self, index: usize) -> impl DoubleEndedIterator<Item = &Node<T>> {
        self.nodes.get(index).unwrap().children.iter().map(|&i| self.nodes.get(i).unwrap())
    }
}

impl<T> Node<T> {
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

impl<T: Clone> Clone for VecTree<T> {
    fn clone(&self) -> Self {
        VecTree {
            nodes: self.nodes.clone(),
            borrows: Cell::new(0),
            root: self.root
        }
    }
}

impl<T: Clone> Clone for Node<T> {
    fn clone(&self) -> Self {
        Node {
            data: UnsafeCell::new(unsafe { (*self.data.get()).clone() }),
            children: self.children.clone()
        }
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
// Iterators

pub struct VecTreeIter<TData> {
    stack: Vec<VisitNode<usize>>,
    next: Option<VisitNode<usize>>,
    data: TData
}

pub trait TreeDataIter {
    type TProxy;
    fn get_children(&self, index: usize) -> &[usize];
    fn get_size(&self) -> usize;
    fn create_proxy(&self, index: usize) -> Self::TProxy;
}

impl<'a, TData: TreeDataIter> Iterator for VecTreeIter<TData> {
    type Item = TData::TProxy;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node_dir) = self.next {
            let index_option = match node_dir {
                VisitNode::Down(index) => {
                    let children = self.data.get_children(index);
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
                return Some(self.data.create_proxy(index));
            }
        }
        None
    }
}

impl<'a, T> VecTree<T> {
    pub fn iter_depth_simple(&'a self) -> VecTreeIter<IterDataSimple<'a, T>> {
        VecTreeIter::<IterDataSimple<'a, T>>::new(self)
    }

    pub fn iter_depth(&self) -> VecTreeIter<IterData<'a, T>> {
        VecTreeIter::<IterData<'a, T>>::new(&self)
    }

    pub fn iter_depth_simple_mut(&'a mut self) -> VecTreeIter<IterDataSimpleMut<'a, T>> {
        VecTreeIter::<IterDataSimpleMut<'a, T>>::new(self)
    }

    pub fn iter_depth_mut(&'a mut self) -> VecTreeIter<IterDataMut<'a, T>> {
        VecTreeIter::<IterDataMut<'a, T>>::new(self)
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
        *self.borrows.get_mut() = 0;
        self.root = None;
    }
}

// ---------------------------------------------------------------------------------------------
// Immutable iterator

impl<'a, T> VecTreeIter<IterDataSimple<'a, T>> {
    fn new(tree: &'a VecTree<T>) -> Self {
        VecTreeIter {
            stack: Vec::new(),
            next: tree.root.map(|id| VisitNode::Down(id)),
            data: IterDataSimple { tree },
        }
    }
}

pub struct IterDataSimple<'a, T> {
    tree: &'a VecTree<T>,
}

impl<'a, T> TreeDataIter for IterDataSimple<'a, T> {
    type TProxy = NodeProxySimple<'a, T>;

    fn get_children(&self, index: usize) -> &[usize] {
        assert!(index < self.tree.len());
        unsafe { &(*self.tree.nodes.as_ptr().add(index)).children }
    }

    fn get_size(&self) -> usize {
        self.tree.len()
    }

    fn create_proxy(&self, index: usize) -> Self::TProxy {
        assert!(index < self.tree.len());
        return NodeProxySimple {
            index,
            data: unsafe { NonNull::new_unchecked((*self.tree.nodes.as_ptr().add(index)).data.get()) },
            _marker: PhantomData
        }
    }
}

pub struct NodeProxySimple<'a, T> {
    pub index: usize,
    data: NonNull<T>,
    _marker: PhantomData<&'a T>
}

impl<T> Deref for NodeProxySimple<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.data.as_ref() }
    }
}

// -- with children

impl<'a, T> VecTreeIter<IterData<'a, T>> {
    fn new(tree: &VecTree<T>) -> Self {
        VecTreeIter {
            stack: Vec::new(),
            next: tree.root.map(|id| VisitNode::Down(id)),
            data: IterData {
                tree_nodes_ptr: tree.nodes.as_ptr(),
                tree_size: tree.nodes.len(),
                _marker: PhantomData
            },
        }
    }
}

pub struct IterData<'a, T> {
    tree_nodes_ptr: *const Node<T>,
    tree_size: usize,
    _marker: PhantomData<&'a T>
}

impl<'a, T> TreeDataIter for IterData<'a, T> {
    type TProxy = NodeProxy<'a, T>;

    fn get_children(&self, index: usize) -> &[usize] {
        assert!(index < self.tree_size);
        unsafe {
            &self.tree_nodes_ptr.add(index).as_ref().unwrap().children
        }
    }

    fn get_size(&self) -> usize {
        self.tree_size
    }

    fn create_proxy(&self, index: usize) -> Self::TProxy {
        assert!(index < self.tree_size);
        return NodeProxy {
            index,
            data: unsafe { NonNull::new_unchecked((*self.tree_nodes_ptr.add(index)).data.get()) },
            tree_node_ptr: self.tree_nodes_ptr,
            tree_size: self.tree_size,
            _marker: PhantomData
        }
    }
}

pub struct NodeProxy<'a, T> {
    pub index: usize,
    data: NonNull<T>,
    tree_node_ptr: *const Node<T>,
    tree_size: usize,
    _marker: PhantomData<&'a T>
}

impl<'a, T> NodeProxy<'a, T> {
    pub fn num_children(&self) -> usize {
        let children = unsafe { &(*self.tree_node_ptr.add(self.index)).children };
        children.len()
    }

    pub fn iter_children(&self) -> impl DoubleEndedIterator<Item=NodeProxy<'_, T>> {
        let children = unsafe { &(*self.tree_node_ptr.add(self.index)).children };
        children.iter().map(|&index| {
            assert!(index < self.tree_size);
            NodeProxy {
                index,
                data: unsafe { NonNull::new_unchecked((*self.tree_node_ptr.add(index)).data.get()) },
                tree_node_ptr: self.tree_node_ptr,
                tree_size: self.tree_size,
                _marker: PhantomData,
            }
        })
    }

    pub fn iter_children_simple(&self) -> impl DoubleEndedIterator<Item=&T> {
        let children = unsafe { &(*self.tree_node_ptr.add(self.index)).children };
        children.iter().map(|&c| unsafe { &*(*self.tree_node_ptr.add(c)).data.get() })
    }

    pub fn iter_depth_simple(&'a self) -> VecTreeIter<IterData<'a, T>> {
        VecTreeIter {
            stack: Vec::new(),
            next: Some(VisitNode::Down(self.index)),
            data: IterData {
                tree_nodes_ptr: self.tree_node_ptr,
                tree_size: self.tree_size,
                _marker: PhantomData
            },
        }
    }
}

impl<T> Deref for NodeProxy<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.data.as_ref() }
    }
}

// ---------------------------------------------------------------------------------------------
// Mutable iterator

impl<'a, T> VecTreeIter<IterDataSimpleMut<'a, T>> {
    fn new(tree: &'a mut VecTree<T>) -> Self {
        VecTreeIter {
            stack: Vec::new(),
            next: tree.root.map(|id| VisitNode::Down(id)),
            data: IterDataSimpleMut { tree },
        }
    }
}

pub struct IterDataSimpleMut<'a, T> {
    tree: &'a mut VecTree<T>,
}

impl<'a, T> TreeDataIter for IterDataSimpleMut<'a, T> {
    type TProxy = NodeProxySimpleMut<'a, T>;

    fn get_children(&self, index: usize) -> &[usize] {
        assert!(index < self.tree.len());
        unsafe { &(*self.tree.nodes.as_ptr().add(index)).children }
    }

    fn get_size(&self) -> usize {
        self.tree.len()
    }

    fn create_proxy(&self, index: usize) -> Self::TProxy {
        assert!(index < self.tree.len());
        return NodeProxySimpleMut {
            index,
            data: unsafe { NonNull::new_unchecked((*self.tree.nodes.as_ptr().add(index)).data.get()) },
            _marker: PhantomData
        }
    }
}

pub struct NodeProxySimpleMut<'a, T> {
    pub index: usize,
    data: NonNull<T>,
    _marker: PhantomData<&'a mut T>     // must be invariant for T
}

impl<T> Deref for NodeProxySimpleMut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.data.as_ref() }
    }
}

impl<T> DerefMut for NodeProxySimpleMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.data.as_mut() }
    }
}

// -- with children

impl<'a, T> VecTreeIter<IterDataMut<'a, T>> {
    fn new(tree: &'a mut VecTree<T>) -> Self {
        VecTreeIter {
            stack: Vec::new(),
            next: tree.root.map(|id| VisitNode::Down(id)),
            data: IterDataMut {
                tree_nodes_ptr: tree.nodes.as_mut_ptr(),
                tree_size: tree.nodes.len(),
                borrows: &tree.borrows,
                _marker: PhantomData
            },
        }
    }
}

pub struct IterDataMut<'a, T> {
    tree_nodes_ptr: *mut Node<T>,
    tree_size: usize,
    borrows: &'a Cell<u32>,
    _marker: PhantomData<&'a mut T>     // must be invariant for T
}

impl<'a, T> TreeDataIter for IterDataMut<'a, T> {
    type TProxy = NodeProxyMut<'a, T>;

    fn get_children(&self, index: usize) -> &[usize] {
        assert!(index < self.tree_size);
        unsafe {
            &self.tree_nodes_ptr.add(index).as_ref().unwrap().children
        }
    }

    fn get_size(&self) -> usize {
        self.tree_size
    }

    fn create_proxy(&self, index: usize) -> Self::TProxy {
        let c = self.borrows.get() + 1;
        self.borrows.set(c);
        assert!(index < self.tree_size);
        return NodeProxyMut {
            index,
            data: unsafe { NonNull::new_unchecked((*self.tree_nodes_ptr.add(index)).data.get()) },
            tree_node_ptr: self.tree_nodes_ptr,
            tree_size: self.tree_size,
            borrows: self.borrows,
            _marker: PhantomData
        }
    }
}

pub struct NodeProxyMut<'a, T> {
    pub index: usize,
    data: NonNull<T>,
    tree_node_ptr: *const Node<T>,
    tree_size: usize,
    borrows: &'a Cell<u32>,
    _marker: PhantomData<&'a mut T>     // must be invariant for T
}

impl<'a, T> NodeProxyMut<'a, T> {
    pub fn num_children(&self) -> usize {
        let children = unsafe { &(*self.tree_node_ptr.add(self.index)).children };
        children.len()
    }

    pub fn iter_children(&self) -> impl DoubleEndedIterator<Item = NodeProxy<'_, T>> {
        let c = self.borrows.get();
        assert!(c <= 1, "{} extra pending mutable reference(s) on children when requesting immutable references on them", c - 1);
        let children = unsafe { &(*self.tree_node_ptr.add(self.index)).children };
        children.iter().map(|&index| {
            assert!(index < self.tree_size);
            NodeProxy {
                index,
                data: unsafe { NonNull::new_unchecked((*self.tree_node_ptr.add(index)).data.get()) },
                tree_node_ptr: self.tree_node_ptr,
                tree_size: self.tree_size,
                _marker: PhantomData,
            }
        })
    }

    pub fn iter_children_simple(&self) -> impl DoubleEndedIterator<Item=&T> {
        let children = unsafe { &(*self.tree_node_ptr.add(self.index)).children };
        children.iter().map(|&c| unsafe { &*(*self.tree_node_ptr.add(c)).data.get() })
    }

    pub fn iter_depth_simple(&'a self) -> VecTreeIter<IterData<'a, T>> {
        VecTreeIter {
            stack: Vec::new(),
            next: Some(VisitNode::Down(self.index)),
            data: IterData {
                tree_nodes_ptr: self.tree_node_ptr,
                tree_size: self.tree_size,
                _marker: PhantomData
            },
        }
    }
}

impl<T> Deref for NodeProxyMut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.data.as_ref() }
    }
}

impl<T> DerefMut for NodeProxyMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.data.as_mut() }
    }
}

impl<T> Drop for NodeProxyMut<'_, T> {
    fn drop(&mut self) {
        let c = self.borrows.get() - 1;
        self.borrows.set(c);
    }
}

// ---------------------------------------------------------------------------------------------
