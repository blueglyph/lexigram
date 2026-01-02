// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexi_gram::lexigram_lib::{TokenId, indent_source, CollectJoin};

pub mod build_lexi;
pub mod build_gram;

// package name & version
pub const STAGE1_PKG_NAME: &str = env!("CARGO_PKG_NAME");
pub const STAGE1_PKG_VERSION: &str = env!("CARGO_PKG_VERSION");

fn gen_hooks_source_code(terminal_hooks: &[TokenId], indent: usize) -> String {
    let mut source = vec![
        format!("static TERMINAL_HOOKS: [TokenId; {}] = [", terminal_hooks.len())
    ];
    source.extend(terminal_hooks.chunks(30).map(|v| format!("    {}", v.into_iter().map(|t| t.to_string()).join(", "))));
    source.push("];".to_string());
    indent_source(vec![source], indent)
}
