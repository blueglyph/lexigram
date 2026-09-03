// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use lexi_gram::lexigram_lib::log::BufLog;
use lexi_gram::options::Options;

pub mod parser_ll1;
pub mod parser_lalr;
mod test;

#[derive(Clone, Debug)]
pub struct ConfigResult {
    pub options: Options,
    pub log: BufLog,
}