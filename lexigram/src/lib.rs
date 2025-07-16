// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub mod lexi;
pub mod gram;

// Main object:
pub use lexi::Lexi;
pub use gram::Gram;

// Exports the version of the lib compatible with this module:
pub use lexigram_lib;

// package name & version
pub const LEXIGRAM_PKG_NAME: &str = env!("CARGO_PKG_NAME");
pub const LEXIGRAM_PKG_VERSION: &str = env!("CARGO_PKG_VERSION");
