// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub mod build_gramlexer;
pub mod build_gramparser;

// const GRAMLEXER_LEXICON: &str = "./src/build_gram/gram_lexicon.l";
// const GRAMPARSER_GRAMMAR: &str = "./src/build_gram/gram_grammar.g";
const GRAMLEXER_FILENAME: &str = "../lexi-gram/src/gram/gramlexer.rs";
const GRAMPARSER_FILENAME: &str = "../lexi-gram/src/gram/gramparser.rs";
const GRAM_TPL_FILENAME: &str = "../lexi-gram/src/gram_template.txt";

const GRAMLEXER_TAG: &str = "gramlexer";
const GRAMPARSER_TAG: &str = "gramparser";
const TPL_LISTENER_TAG: &str = "listener";
const TPL_TYPES_TAG: &str = "user_types";

// const BUILD_GRAMPARSER_FILENAME: &str = "./src/build_gram/build_gramparser.rs";
// const GRAM_SYM_T_TAG: &str = "terminal_symbols";
