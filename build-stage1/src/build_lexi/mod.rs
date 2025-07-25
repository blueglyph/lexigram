// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

pub mod build_lexilexer;
pub mod build_lexiparser;

const LEXILEXER_LEXICON: &str = "./src/build_lexi/lexi_lexicon.l";
const LEXIPARSER_GRAMMAR: &str = "./src/build_lexi/lexi_grammar.g";
const LEXILEXER_STAGE2_FILENAME: &str = "../build-stage2/src/build_lexi/build_lexilexer.rs";
const LEXIPARSER_STAGE2_FILENAME: &str = "../build-stage2/src/build_lexi/build_lexiparser.rs";

const LEXILEXER_STAGE2_TAG: &str = "lexilexer_stage_2";
const LEXIPARSER_STAGE2_TAG: &str = "lexiparser_stage_2";

const BUILD_LEXIPARSER_FILENAME: &str = "./src/build_lexi/build_lexiparser.rs";
const LEXI_SYM_T_TAG: &str = "terminal_symbols";

const VERSIONS_TAG: &str = "versions";