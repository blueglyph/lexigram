pub mod build_lexilexer;
pub mod build_lexiparser;

const LEXILEXER_LEXICON: &str = "./src/lexicon.l";
const LEXILEXER_FILENAME: &str = "../lexi/src/lexilexer.rs";
const LEXIPARSER_FILENAME: &str = "../lexi/src/lexiparser.rs";

const LEXILEXER_TAG: &str = "lexilexer";
const LEXIPARSER_TAG: &str = "lexiparser";

const BUILD_LEXIPARSER_FILENAME: &str = "./src/build_lexiparser.rs";
const LEXI_SYM_T_TAG: &str = "terminal_symbols";