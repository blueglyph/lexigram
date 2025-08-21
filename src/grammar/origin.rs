// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

use std::collections::HashMap;
use std::marker::PhantomData;
use crate::grammar::{GrTree, VarId};

#[derive(Clone, Debug)]
pub struct FromRTS;
// #[derive(Clone, Debug)]
// pub struct FromPRS;

#[derive(Clone, Debug)]
pub struct Origin<T> {
    pub(crate) trees: Vec<GrTree>,
    pub(crate) map: HashMap<(VarId, usize), (VarId, usize)>,
    _phantom: PhantomData<T>,
}

impl<T> Origin<T> {
    pub fn new() -> Self {
        Origin {
            trees: Vec::new(),
            map: HashMap::new(),
            _phantom: PhantomData,
        }
    }
    
    pub fn with_capacity(size: usize) -> Self {
        Origin {
            trees: Vec::with_capacity(size),
            map: HashMap::new(),
            _phantom: PhantomData,
        }
    }

    // pub fn add_tree(&mut self, tree: GrTree) {
    //     self.trees.push(tree);
    // }
    
    // pub fn set_trees<'a, I: IntoIterator<Item = &'a GrTree>>(&mut self, iter_tree: I) {
    //     self.trees = iter_tree.into_iter().cloned().collect()
    // }

    /// Sets the original [`tree`](GrTree) of nonterminal `var`.
    pub fn set_tree(&mut self, var: VarId, tree: GrTree) {
        let var = var as usize;
        if self.trees.len() <= var {
            self.trees.resize(var + 1, GrTree::new());
        } 
        self.trees[var] = tree;
    }
    
    /// Gets the original [`GrTree`] of nonterminal `var`, if it exists.
    pub fn get_tree(&self, var: VarId) -> Option<&GrTree> {
        self.trees.get(var as usize)
    } 
}

impl Origin<FromRTS> {
    /// Adds a connection between a `new` node and its `origin` in the original [`GrTree`].
    /// 
    /// `new` is made up of a nonterminal index and its tree index.
    pub fn add(&mut self, new: (VarId, usize), origin: (VarId, usize)) {
        self.map.insert(new, origin);
    }
    
    /// Gets the original [`GrTree`] and node index, if they exist, from a `new` node. 
    /// 
    /// `new` is made up of a nonterminal index and its tree index.
    pub fn get(&self, var: VarId, new: usize) -> Option<(&GrTree, usize)> {
        self.map.get(&(var, new)).cloned()
            .map(|(v, node)| (&self.trees[v as usize], node))
    }
}
