#![allow(dead_code)]
#![allow(unused)]

mod tests;

use std::fmt::{Display, Formatter};
use std::ops::Deref;
use crate::cproduct::CProduct;
use crate::dfa::TokenId;
use crate::{CollectJoin, gnode, vaddi};
use crate::vectree::VecTree;

pub type VarId = u16;

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub enum Symbol {
    #[default] Empty,
    T(TokenId),
    NT(VarId)
}

#[derive(Clone, PartialEq, Debug)]
pub enum GrNode {
    Symbol(Symbol),
    Concat,
    Or,
    Maybe,
    Plus,
    Star,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Empty => write!(f, "ε"),
            Symbol::T(id) => write!(f, "[{id}]"),
            Symbol::NT(id) => write!(f, "{id}"),
        }
    }
}

impl Display for GrNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GrNode::Symbol(s) => write!(f, "{s}"),
            GrNode::Concat => write!(f, "&"),
            GrNode::Or => write!(f, "|"),
            GrNode::Maybe => write!(f, "?"),
            GrNode::Plus => write!(f, "+"),
            GrNode::Star => write!(f, "*"),
        }
    }
}

impl Symbol {
    pub fn to_str(&self, symbol_table: &[(String, Option<String>)]) -> String {
        match self {
            Symbol::Empty => "ε".to_string(),
            Symbol::T(id) => format!("'{}'", symbol_table[*id as usize].1.as_ref().unwrap()),
            Symbol::NT(id) => format!("{}", symbol_table[*id as usize].0),
        }
    }
}

impl GrNode {
    pub fn to_str(&self, symbol_table: &[(String, Option<String>)]) -> String {
        match self {
            GrNode::Symbol(s) => s.to_str(symbol_table),
            _ => self.to_string()
        }
    }
}

// ---------------------------------------------------------------------------------------------

/// Simple index object that returns `Original(<value>)` on the first `index.get()`, then
/// `Copy(<value>)` on subsequent calls. The indices are stored on 31 bits, keeping one bit
/// for the 'original' flag. Trying to store larger values triggers a panic.
#[derive(Clone, Copy)]
struct Dup {
    index: u32
}

#[derive(Clone, Copy, Debug)]
enum DupVal{
    Original(u32),
    Copy(u32)
}

impl Dup {
    const MASK: u32 = 1 << (u32::BITS - 1);

    fn new(index: usize) -> Self {
        assert!(index < Self::MASK as usize);
        Self { index: index as u32 }
    }

    fn get(&mut self) -> DupVal {
        let idx = self.index;
        if idx & Self::MASK == 0 {
            self.index |= Self::MASK;
            DupVal::Original(idx)
        } else {
            DupVal::Copy(idx & !Self::MASK)
        }
    }

    fn peek(&self) -> usize {
        (self.index & !Self::MASK) as usize
    }
}

impl std::fmt::Debug for Dup {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Dup{{")?;
        if self.index & Self::MASK == 0 {
            write!(f, "{}}}", self.index)
        } else {
            write!(f, "copy {}}}", self.index & !Dup::MASK)
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct RuleTree(VecTree<GrNode>);

impl RuleTree {
    fn new() -> Self {
        RuleTree(VecTree::new())
    }

    fn get_dup(&mut self, dup_index: &mut Dup) -> usize {
        match dup_index.get() {
            DupVal::Original(index) => index as usize,
            DupVal::Copy(index) => {
                let node = self.0.get(index as usize).clone();
                self.0.add(None, node)
            }
        }
    }

    /// Transforms the production rule tree into a list of rules in normalized format:
    /// `var -> &(leaf_1, leaf_2, ...leaf_n)`
    ///
    /// The product may have to be split if operators like `+` or `*` are used. In this
    /// case, new non-terminals are created, with increasing IDs starting from
    /// `next_var_id`.
    fn normalize(self, var_id: VarId, next_var_id: VarId) -> Vec<(VarId, Self)> {
        const VERBOSE: bool = false;
        const VERBOSE_CC: bool = false;
        let mut new = RuleTree::new();
        let mut rules = Vec::<(VarId, RuleTree)>::new();
        let mut stack = Vec::<usize>::new();                // indices in new
        for sym in self.0.iter_depth() {
            let n = sym.num_children();
            if VERBOSE { println!("- old {}:{}", sym.index, sym.deref()); }
            if n == 0 {
                stack.push(new.0.add(None, self.0.get(sym.index).clone()));
                if VERBOSE { print!("  leaf: "); }
            } else {
                match sym.deref() {
                    GrNode::Concat | GrNode::Or => {
                        // we must rearrange the operations so that any item on the stack is only
                        // one of those combinations:
                        // - a leaf
                        // - a &(leaves)
                        // - a |(&(leaves) or leaves)
                        let children = stack.drain(stack.len() - n..).to_vec();
                        let new_id = if children.iter().all(|&idx| !matches!(new.0.get(idx), GrNode::Concat|GrNode::Or)) {
                            if VERBOSE { print!("  trivial {}: children={children:?}\n  ", sym.deref()); }
                            // trivial case with only leaves as children (could be removed and treated as a general case)
                            new.0.addci_iter(None, sym.clone(), children)
                        } else {
                            if let GrNode::Or = sym.deref() {
                                if VERBOSE { println!("  or: children={children:?}"); }
                                // if parent sym is p:|
                                // - preserving the children's order:
                                //   - attach '|' children's children directly under p (discarding the '|' children)
                                //   - attach '&' children under p
                                // - push p back to stack
                                // ex: P: AB | (C|D) | E | (FG|HI)             -> P: AB | C | D | E | FG | HI
                                //        |(&(A,B),|(C,D),E,|(&(F,G),&(H,I)))        |(&(A,B),C,D,E,&(F,G),&(H,I))
                                let mut new_children = Vec::new();
                                for id in children {
                                    match new.0.get(id) {
                                        GrNode::Symbol(_) | GrNode::Concat => {
                                            if VERBOSE { println!("  - child {id} is {}", new.0.get(id)); }
                                            new_children.push(id);
                                        }
                                        GrNode::Or => {
                                            if VERBOSE { println!("  - child {id} is | with children {:?}", new.0.children(id)); }
                                            new_children.extend(new.0.children(id));
                                        }
                                        x => panic!("unexpected node type under | node: {x:?}"),
                                    }
                                }
                                new.0.addci_iter(None, gnode!(|), new_children)
                            } else { // GrNode::Concat
                                if VERBOSE_CC { println!("  &: children={children:?}"); }
                                // if parent sym is p:&
                                // - merge adjacent leaves and '&' children (optional)
                                // - cartesian product of all '|' children's children and '&' children,
                                //       duplicating nodes are required
                                // - add r:'|' node to tree, attaching the new '&' nodes under it
                                // - push r to stack
                                // ex: P: AB & (C|D) & E & (FG|H)        -> P: ABCEFG | ABCEH | ABDEFG | ABDEH
                                //        &(&(A,B),|(C,D),E,|(&(F,G),H))      |(&(A,B,C,E,F,G),&(A,B,C,E,H),&(A,B,D,E,F,G),&(A,B,D,E,H)

                                // we store the dups in an array and reference them by index, because there will be multiple instances
                                // pointing to the same Dup and we can't do that with mutable references (which must be unique):
                                let mut dups = Vec::<Vec<Dup>>::new();
                                let concats_children = children.into_iter()
                                    // iterations: &(A,B) -> |(C,D) -> E -> |(&(F,G),H))
                                    .flat_map(|id| {
                                        if VERBOSE_CC { print!("      FL {}: ", new.0.get(id)); }
                                        match new.0.get(id) {
                                            GrNode::Concat =>
                                                new.0.children(id).iter().map(|idc| vec![vaddi(&mut dups, [Dup::new(*idc)])]).to_vec(),
                                            GrNode::Or => {
                                                let children = new.0.children(id).to_vec();
                                                vec![children.into_iter().map(|idc| {
                                                    if let GrNode::Concat = new.0.get(idc) {
                                                        let idc_children = new.0.children(idc).iter().map(|i| Dup::new(*i)).to_vec();
                                                        vaddi(&mut dups, idc_children)
                                                    } else {
                                                        vaddi(&mut dups, [Dup::new(idc)])
                                                    }
                                                }).to_vec()]
                                            }
                                            _ =>
                                                vec![vec![vaddi(&mut dups, [Dup::new(id)])]],
                                        }
                                    })
                                    // [d(A)] -> [d(B)] -> [d(C),d(D)] -> [d(E)] -> [d(&(d(F),d(G))),d(H)]
                                    // .inspect(|x| println!("      >> {}", x.iter().map(|i| format!("_{i}")).join(", ")))
                                    .cproduct()
                                    // .inspect(|x| println!("      << {}", x.iter().map(|i| format!("_{i}")).join(", ")))
                                    // [dup(A),dup(B),dup(C),dup(E),d(&)] -> [dup(A),dup(B),dup(C),dup(E),d(H)] ->
                                    //       [dup(A),dup(B),dup(D),dup(E),d(&)] -> [dup(A),dup(B),dup(D),dup(E),d(H)]
                                    .map(|dup_ids| dup_ids.into_iter()
                                        .flat_map(|dup_id| dups.get_mut(dup_id).unwrap().iter_mut()
                                            .map(|dup| new.get_dup(dup)).to_vec()).to_vec()
                                    )
                                    // .inspect(|x| println!("      :: {}", x.iter().map(|i| format!("{i}")).join(", ")))
                                    .to_vec();
                                    // [A,B,C,E,F,G] -> [A',B',C',E',H] -> [A'',B'',D,E'',F',G'] -> [A''',B''',D',E''',H']
                                let concats = concats_children.into_iter()
                                    .map(|children_ids| new.0.addci_iter(None, gnode!(&), children_ids))
                                    .to_vec();
                                    // Vec<node id of &-branch>
                                new.0.addci_iter(None, gnode!(|), concats)
                            }
                        };
                        stack.push(new_id);
                    }
                    GrNode::Maybe => {
                        if VERBOSE { print!("  ?: "); }
                        assert_eq!(n, 1);
                        let empty = new.0.add(None, gnode!(e));
                        let id = new.0.addci_iter(None, gnode!(|), [stack.pop().unwrap(), empty]);
                        stack.push(id);
                    }
                    GrNode::Plus => {
                        assert_eq!(n, 1);
                        // create new production rule:
                        // P -> αβ+γ becomes P -> αQγ
                        //                   Q -> βQ | β
                        todo!()
                    }
                    GrNode::Star => {
                        assert_eq!(n, 1);
                        // create new production rule:
                        // P -> αβ*γ becomes P -> αQγ
                        //                   Q -> βQ | ε
                        todo!()
                    }
                    _ => panic!("Unexpected {}", sym.deref())
                }
            }
            if VERBOSE_CC {
                println!("stack: {}", stack.iter()
                    .map(|id| {
                        let children = new.0.children(*id);
                        format!("{id}:{}{}", new.0.get(*id), if children.is_empty() { "".to_string() } else { format!("({})", children.iter().join(",")) })
                    }).join(", ")
                );
            }
        }
        assert_eq!(stack.len(), 1);
        if VERBOSE_CC { println!("Final stack id: {}", stack[0]); }
        new.0.set_root(stack.pop().unwrap());
        rules.push((var_id, new));
        rules
    }
}

impl Display for RuleTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn snode(show_ids: bool, show_depth: bool, node: &GrNode, node_id: usize, depth: u32) -> String {
            let mut result = String::new();
            if show_depth {
                result.push_str(&depth.to_string());
                result.push('>');
            }
            if show_ids {
                result.push_str(&node_id.to_string());
                result.push(':');
            }
            result.push_str(&node.to_string());
            result
        }
        let show_ids = f.alternate();
        let show_depth = f.sign_plus();
        let mut stack = Vec::<String>::new();
        for node in self.0.iter_depth() {
            let n = node.num_children();
            if n > 0 {
                let children = stack.drain(stack.len() - n..).join(", ");
                stack.push(format!("{}({children})", snode(show_ids, show_depth, &node, node.index, node.depth)));
            } else {
                stack.push(snode(show_ids, show_depth, &node, node.index, node.depth));
            }
        }
        write!(f, "{}", stack.pop().unwrap_or("empty".to_string()))
    }
}

// ---------------------------------------------------------------------------------------------

pub struct GrammarBuilder {
    rules: Vec<RuleTree>,
    symbols: Vec<(String, Option<String>)>
}

impl GrammarBuilder {
    pub fn new() -> Self {
        GrammarBuilder {
            rules: Vec::new(),
            symbols: Vec::new()
        }
    }

    pub fn from(parsed_rules: Vec<RuleTree>) -> Self {
        GrammarBuilder {
            rules: parsed_rules,
            symbols: Vec::new()
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates a `GrNode` instance.
    ///
    /// # Examples
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::gnode;
    /// # use rlexer::grammar::{GrNode, Symbol, VarId};
    /// assert_eq!(gnode!([1]), GrNode::Symbol(Symbol::T(1 as TokenId)));
    /// assert_eq!(gnode!(t 2), GrNode::Symbol(Symbol::T(2 as TokenId)));
    /// assert_eq!(gnode!(nt 3), GrNode::Symbol(Symbol::NT(3 as VarId)));
    /// assert_eq!(gnode!(e), GrNode::Symbol(Symbol::Empty));
    /// assert_eq!(gnode!(&), GrNode::Concat);
    /// assert_eq!(gnode!(|), GrNode::Or);
    /// assert_eq!(gnode!(?), GrNode::Maybe);
    /// assert_eq!(gnode!(+), GrNode::Plus);
    /// assert_eq!(gnode!(*), GrNode::Star);
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! gnode {
        ([$id:expr]) => { gnode!(t $id) };
        (t $id:expr) => { GrNode::Symbol(Symbol::T($id as TokenId)) };
        (nt $id:expr) => { GrNode::Symbol(Symbol::NT($id as VarId)) };
        (e) => { GrNode::Symbol(Symbol::Empty) };
        //
        (&) => { GrNode::Concat };
        (|) => { GrNode::Or };
        (?) => { GrNode::Maybe };
        (+) => { GrNode::Plus };
        (*) => { GrNode::Star };
    }

}
