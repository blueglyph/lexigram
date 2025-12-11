// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;
use crate::VarId;
use crate::grammar::GrTree;

#[derive(Clone, Debug)]
/// Origin from a RuleTreeSet perspective: [`Origin<(VarId, usize), FromRTS>`](Origin), where
/// `(VarId, usize)` identifies a node in the processed [`RuleTreeSet`]:
/// * [`VarId`] is the variable
/// * `usize` is the node index in the [`GrTree`] of that variable
///
/// and associates it with a node [`VarId`, usize] of the original [`RuleTreeSet`] rules
/// after normalization (but keeping the * and + ops).
pub struct FromRTS;

#[derive(Clone, Debug)]
/// Origin from a ProdRuleSet perspective: [`Origin<VarId, FromPRS>`](Origin), where
/// `VarId` identifies a children nonterminal in the processed [`ProdRuleSet`]:
/// * [`VarId`] is the variable
///
/// and associates it with a node [`VarId`, usize] of the original [`RuleTreeSet`] rules
/// after normalization (but keeping the * and + ops).
///
/// The alternatives of the ProdRuleSet store their own [`VarId`, usize] links to the original
/// nodes because it's easier than to track the data when they're moved around.
pub struct FromPRS;

#[derive(Clone, Debug)]
pub struct Origin<F, T> {
    pub trees: Vec<GrTree>,
    pub map: HashMap<F, (VarId, usize)>,
    _phantom: PhantomData<T>,
}

impl<F, T> Origin<F, T> {
    /// Creates a blank [`Origin`] object.
    pub fn new() -> Self {
        Origin {
            trees: Vec::new(),
            map: HashMap::new(),
            _phantom: PhantomData,
        }
    }
    
    /// Creates a blank [`Origin`] object with a tree capacity of `size`. Use this method when
    /// you can't give the data in a form that suits the other constructors but when you know
    /// the number of variables.
    pub fn with_capacity(size: usize) -> Self {
        Origin {
            trees: Vec::with_capacity(size),
            map: HashMap::new(),
            _phantom: PhantomData,
        }
    }

    /// Creates an [`Origin`] object with the given trees and mapping.
    pub fn from_data(trees: Vec<GrTree>, map: HashMap<F, (VarId, usize)>) -> Self {
        Origin { trees, map, _phantom: PhantomData }
    }

    /// Creates an [`Origin`] object with the given trees as mutable reference. It will
    /// take the content from `trees` to create the object. After that, `trees` is empty.
    ///
    /// Use this method to create an [`Origin`] object from another generic form of the
    /// same type; typically when creating an `Origin<(VarId, AltId), FromPRS>` from an
    /// `Origin<(VarId, usize), FromRTS>` if you can't move the original.
    pub fn from_trees_mut(trees: &mut Vec<GrTree>) -> Self {
        let trees = std::mem::take(trees);
        Self::from_data(trees, HashMap::new())
    }

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

impl<F: Eq + Hash, T> Origin<F, T> {
    /// Adds a connection between a `new` node and its `origin` in the original [`GrTree`].
    /// 
    /// `new` is made up of a nonterminal index and its tree index.
    pub fn add(&mut self, new: F, origin: (VarId, usize)) {
        self.map.insert(new, origin);
    }
    
    /// Gets the original [`GrTree`] and node index, if they exist, from a `new` node. 
    /// 
    /// `new` is made up of a nonterminal index and its tree index.
    pub fn get(&self, new: F) -> Option<(&GrTree, usize)> {
        self.map.get(&new).cloned()
            .map(|(v, node)| (&self.trees[v as usize], node))
    }
}
