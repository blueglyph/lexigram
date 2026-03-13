grammar Config;

config:
    definitions
    lexer
    parser
    options
;

definitions:
    (<L=i_def> Def Id Equal value Semicolon)*   // any type
;

lexer:
    Lexer Lbracket io_options Rbracket
;

parser:
    (Parser Lbracket io_options Rbracket)?
;

options:
    (Options Lbracket global_options Rbracket)?
;

io_options:
    io_option (<L=i_io_opt> Comma io_option)*
;

io_option:
    Combined Colon value tag_opt            // string
|   Input    Colon value tag_opt            // string
|   Output   Colon value tag_opt            // string
|   Indent   Colon value                    // num
|   Headers  Colon Lbracket value (Comma value)* Rbracket // string
;

tag_opt:
    (LSbracket value RSbracket)?            // string
;

global_options:
     global_option (Comma global_option)*
;

global_option:
    Headers Colon Lbracket value (Comma value)* Rbracket    // string
|   Indent  Colon value                                     // num
|   Libs    Colon Lbracket value (Comma value)* Rbracket    // string
|   NTValue Colon nt_value
|   Spans   Colon value                                     // bool
;

value:
    BoolLiteral
|   NumLiteral
|   StrLiteral
|   Id
|   Stdout
|   String
;

nt_value:
    Default
|   None
|   Parents
|   Set Lbracket Id (Comma Id)* Rbracket
;

// static LEXICON_FILENAME: &str = "src/watcher.lg";
// static SOURCE_FILENAME: &str = "../watcher/src/lib.rs";
// static LEXER_TAG: &str = "watcher_lexer";
// static PARSER_TAG: &str = "watcher_parser";
// const LEXER_INDENT: usize = 4;
// const PARSER_INDENT: usize = 4;
// 
// fn gen_source_watcher(action: Action) {
//     let options = OptionsBuilder::new()
//         .combined_spec(genspec!(filename: LEXICON_FILENAME))
//         .lexer_code(gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
//         .indent(LEXER_INDENT)
//         // grammar is combined with lexicon, no need to define parser_spec
//         .parser_code(gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
//         .indent(PARSER_INDENT)
//         .set_nt_value(NTValue::None)
//         .span_params(true)
//         .build()
//         .expect("should have no error");
// 
// // -------------------------------------
// 
// def SOURCE_FILENAME = "../watcher/src/lib.rs";
// 
// lexer {
//     combined: "src/watcher.lg",
//     output: SOURCE_FILENAME ["watcher_lexer"],
//     indent: 4;
// }
// parser {
//     output: SOURCE_FILENAME ["watcher_parser"];
//     indent: 4;
// }
// options {
//     nt_value: none;
//     spans: true;
// }
// 
// // -------------------------------------------------------------------------
// 
// static LEXICON_FILENAME: &str = "src/microcalc.l";
// static GRAMMAR_FILENAME: &str = "src/microcalc.g";
// static LEXICON_GRAMMAR_FILENAME: &str = "src/microcalc.lg";
// static SOURCE_FILENAME: &str = "../microcalc/src/main.rs";
// static LEXER_TAG: &str = "microcalc_lexer";
// static PARSER_TAG: &str = "microcalc_parser";
// const LEXER_INDENT: usize = 4;
// const PARSER_INDENT: usize = 4;
// 
// fn gen_source_microcalc_l_g(action: Action) {
//     let options = OptionsBuilder::new()
//         .lexer(genspec!(filename: LEXICON_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
//         .indent(LEXER_INDENT)
//         .parser(genspec!(filename: GRAMMAR_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
//         .indent(PARSER_INDENT)
//         .extra_libs(["super::listener_types::*"])
//         .build()
//         .expect("should have no error");
//     match try_gen_parser(action, options) {
//         Ok(log) => {
//             if action == Action::Generate {
//                 println!("Code generated in {SOURCE_FILENAME}\n{log}");
//             }
//             assert!(log.has_no_warnings(), "unexpected warning(s):\n{}", log.get_warnings().join("\n"));
//         }
//         Err(build_error) => panic!("{build_error}"),
//     }
// }
// 
// // -------------------------------------
// 
// def LEXICON_GRAMMAR_FILENAME = "src/microcalc.lg";
// def SOURCE_FILENAME = "../microcalc/src/main.rs";
// 
// lexer {
//     input: LEXICON_GRAMMAR_FILENAME;
//     output: SOURCE_FILENAME ["microcalc_lexer"];
//     indent: 4;
// }
// parser {
//     input: LEXICON_GRAMMAR_FILENAME;
//     output: SOURCE_FILENAME ["microcalc_parser"];
//     indent: 4;
// }
// options {
//     libs: { "super::listener_types::*" };
//     nt_value: default;
// }
// 
// // -------------------------------------------------------------------------
