// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub mod build_lexilexer;
pub mod build_lexiparser;

// const LEXILEXER_LEXICON: &str = "./src/build_lexi/lexi_lexicon.l";
// const LEXIPARSER_GRAMMAR: &str = "./src/build_lexi/lexi_grammar.g";
const LEXILEXER_FILENAME: &str = "../lexi-gram/src/lexi/lexilexer.rs";
const LEXIPARSER_FILENAME: &str = "../lexi-gram/src/lexi/lexiparser.rs";
const LEXI_TPL_FILENAME: &str = "../lexi-gram/src/lexi_template.txt";

const LEXILEXER_TAG: &str = "lexilexer";
const LEXIPARSER_TAG: &str = "lexiparser";
const TPL_LISTENER_TAG: &str = "listener";
const TPL_TYPES_TAG: &str = "user_types";

// const BUILD_LEXIPARSER_FILENAME: &str = "./src/build_lexi/build_lexiparser.rs";
// const LEXI_SYM_T_TAG: &str = "terminal_symbols";
