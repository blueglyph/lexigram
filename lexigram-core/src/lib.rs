// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub mod alt;
pub mod segmap;
pub mod char_reader;
pub mod fixed_sym_table;
pub mod log;
pub mod lexer;
pub mod parser;
pub mod text_span;

// package name & version
pub const CORE_PKG_NAME: &str = env!("CARGO_PKG_NAME");
pub const CORE_PKG_VERSION: &str = env!("CARGO_PKG_VERSION");

/// ID of a lexer token
pub type TokenId = u16;
/// ID of a nonterminal
pub type VarId = u16;
/// ID of a rule alternative. We use the same type as [VarId] because they're very similar quantities.
pub type AltId = VarId;

#[derive(Clone, Debug)]
pub struct LL1;

#[derive(Clone, Debug)]
pub struct LALR;

// ---------------------------------------------------------------------------------------------
// General helper traits

pub trait CharLen {
    /// Returns the length in characters (not bytes).
    fn charlen(&self) -> usize;
}

impl<T: AsRef<str>> CharLen for T {
    fn charlen(&self) -> usize {
        self.as_ref().chars().count()
    }
}

/// This trait provides a shortcut for two commonly used iterator adapters.
///
/// * [join(...)](CollectJoin::join): joins [ToString] items into a `Vec::<String>` by inserting a separator between them.
/// * [to_vec(...)](CollectJoin::to_vec): equivalent to `.collect::<Vec<_>>()`
pub trait CollectJoin {
    /// Iterator adapter that joins [ToString] items into a `Vec::<String>` by inserting `separator` between them.
    ///
    /// ## Example
    ///
    /// ```
    /// # use lexigram_core::CollectJoin;
    /// let numbers = (0..10).filter(|&x| x < 5).join(", ");
    /// assert_eq!(numbers, "0, 1, 2, 3, 4");
    /// ```
    fn join(&mut self, separator: &str) -> String
        where Self: Iterator,
              <Self as Iterator>::Item: ToString
    {
        self.map(|x| x.to_string()).collect::<Vec<_>>().join(separator)
    }

    /// Iterator adapter that joins items into a `Vec<_>`; equivalent to `.collect::<Vec<_>>()`
    ///
    /// ## Example
    ///
    /// ```
    /// # use lexigram_core::CollectJoin;
    /// let values = [1, 5, 10, 25, 50].into_iter().map(|x| x * x).to_vec();
    /// assert_eq!(values, vec![1, 25, 100, 625, 2500]);
    /// ```
    fn to_vec(self) -> Vec<<Self as Iterator>::Item>
        where Self: Iterator + Sized
    {
        self.collect::<Vec<_>>()
    }
}

impl<I: Iterator> CollectJoin for I {}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates an `OpCode` instance.
    ///
    /// # Examples
    /// ```
    /// # use lexigram_core::TokenId;
    /// # use lexigram_core::opcode;
    /// # use lexigram_core::VarId;
    /// # use lexigram_core::parser::OpCode;
    /// assert_eq!(opcode!(e), OpCode::Empty);
    /// assert_eq!(opcode!(t 2), OpCode::T(2 as TokenId));
    /// assert_eq!(opcode!(nt 3), OpCode::NT(3));
    /// assert_eq!(opcode!(loop 2), OpCode::Loop(2));
    /// assert_eq!(opcode!(exit 1), OpCode::Exit(1));
    /// assert_eq!(opcode!(nt 3), OpCode::NT(3));
    /// assert_eq!(opcode!(loop 2), OpCode::Loop(2));
    /// assert_eq!(opcode!(exit 1), OpCode::Exit(1));
    /// assert_eq!(opcode!(hook), OpCode::Hook);
    /// assert_eq!(opcode!(end), OpCode::End);
    #[macro_export]
    macro_rules! opcode {
        (e) => { $crate::parser::OpCode::Empty };
        (t $id:expr) => { $crate::parser::OpCode::T($id as $crate::TokenId) };
        (nt $id:expr) => { $crate::parser::OpCode::NT($id as $crate::VarId) };
        (loop $id:expr) => { $crate::parser::OpCode::Loop($id as $crate::VarId) };
        (exit $id:expr) => { $crate::parser::OpCode::Exit($id as $crate::VarId) };
        (nt $id:expr) => { $crate::parser::OpCode::NT($id as $crate::VarId, 0) };
        (loop $id:expr) => { $crate::parser::OpCode::Loop($id as $crate::VarId, 0) };
        (exit $id:expr) => { $crate::parser::OpCode::Exit($id as $crate::VarId, 0) };
        (hook) => { $crate::parser::OpCode::Hook };
        (end) => { $crate::parser::OpCode::End };
    }

    /// Generates an opcode strip. A strip is made up of `OpCode` items separated by a comma.
    ///
    /// # Example
    /// ```
    /// # use lexigram_core::{TokenId, VarId, strip, opcode};
    /// # use lexigram_core::alt::Alternative;
    /// # use lexigram_core::parser::{OpCode, Symbol};
    /// assert_eq!(strip!(nt 1, loop 5, t 3, e), vec![opcode!(nt 1), opcode!(loop 5), opcode!(t 3), opcode!(e)]);
    /// ```
    #[macro_export]
    macro_rules! strip {
        () => { std::vec![] };
        ($($a:ident $($b:expr)?,)+) => { strip![$($a $($b)?),+] };
        ($($a:ident $($b:expr)?),*) => { std::vec![$($crate::opcode!($a $($b)?)),*] };
    }
}
