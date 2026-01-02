// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub mod build_gramlexer;
pub mod build_gramparser;

const GRAMLEXER_LEXICON: &str = "./src/build_gram/gram_lexicon.l";
const GRAMPARSER_GRAMMAR: &str = "./src/build_gram/gram_grammar.g";
const GRAMLEXER_STAGE2_FILENAME: &str = "../build-stage2/src/build_gram/build_gramlexer.rs";
const GRAMPARSER_STAGE2_FILENAME: &str = "../build-stage2/src/build_gram/build_gramparser.rs";

const GRAMLEXER_STAGE2_TAG: &str = "gramlexer_stage_2";
const GRAMPARSER_STAGE2_TAG: &str = "gramparser_stage_2";
const GRAMPARSER_STAGE2_HOOKS_TAG: &str = "gramparser_stage_2_hooks";

const BUILD_GRAMPARSER_FILENAME: &str = "./src/build_gram/build_gramparser.rs";
const GRAM_SYM_T_TAG: &str = "terminal_symbols";

const VERSIONS_TAG: &str = "versions";
