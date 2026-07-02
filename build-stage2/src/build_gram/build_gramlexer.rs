// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_lib::lexergen::{LexerGen, LexigramCrate};
use lexigram_lib::file_utils::replace_tagged_source;
use lexigram_lib::{branch, btreemap, term, Normalized, SymbolTable};
use lexigram_lib::dfa::{Dfa, DfaTables};
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::build::BuildFrom;
use super::{GRAMLEXER_FILENAME, GRAMLEXER_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 17] = [
    ("Colon",     Some(":")),         // 0
    ("Lparen",    Some("(")),         // 1
    ("Or",        Some("|")),         // 2
    ("Plus",      Some("+")),         // 3
    ("Question",  Some("?")),         // 4
    ("Rparen",    Some(")")),         // 5
    ("Semicolon", Some(";")),         // 6
    ("Sep",       Some("/")),         // 7
    ("Star",      Some("*")),         // 8
    ("Grammar",   Some("grammar")),   // 9
    ("SymEof",    Some("EOF")),       // 10
    ("Lform",     None),              // 11
    ("Rform",     Some("<R>")),       // 12
    ("Pform",     Some("<P>")),       // 13
    ("Greedy",    Some("<G>")),       // 14
    ("ResolveTag",Some("<resolve>")), // 15
    ("Id",        None),              // 16
];

// [terminal_symbols]
// -------------------------------------------------------------------------

const EXPECTED_NBR_WARNINGS: usize = 0;

fn gramlexer_source(indent: usize, _verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram-lib: 0.9.4
    // lexi-gram: 0.9.4
    // build-stage1: 0.9.4

    // [versions]

    // -------------------------------------------------------------------------
    // [gramlexer_stage_2]

    let dfa_tables = DfaTables::new(
        btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 17, '(' => 18, ')' => 19, '*' => 20, '+' => 21, '/' => 22, ':' => 23, ';' => 24, '<' => 1, '?' => 25, 'A'-'D', 'F'-'Z', 'a'-'f', 'h'-'z' => 26, 'E' => 27, 'g' => 28, '|' => 29),
            1 => branch!('G' => 4, 'L' => 5, 'P' => 6, 'R' => 7, 'r' => 8),
            2 => branch!(~['*'] => 2, ['*'] => 3),
            3 => branch!(~['*', '/'] => 2, ['*'] => 3, ['/'] => 31),
            4 => branch!('>' => 43),
            5 => branch!('=' => 9, '>' => 40),
            6 => branch!('>' => 42),
            7 => branch!('>' => 41),
            8 => branch!('e' => 11),
            9 => branch!('A'-'Z', 'a'-'z' => 10),
            10 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 10, '>' => 40),
            11 => branch!('s' => 12),
            12 => branch!('o' => 13),
            13 => branch!('l' => 14),
            14 => branch!('v' => 16),
            15 => branch!('>' => 44),
            16 => branch!('e' => 15),
            17 => branch!('\t'-'\n', '\r', ' ' => 17),
            18 => branch!(),
            19 => branch!(),
            20 => branch!(),
            21 => branch!(),
            22 => branch!('*' => 2, '/' => 30),
            23 => branch!(),
            24 => branch!(),
            25 => branch!(),
            26 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 26),
            27 => branch!('0'-'9', 'A'-'N', 'P'-'Z', '_', 'a'-'z' => 26, 'O' => 38),
            28 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 26, 'r' => 32),
            29 => branch!(),
            30 => branch!(~['\n', '\r'] => 30),
            31 => branch!(),
            32 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 26, 'a' => 33),
            33 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 26, 'm' => 34),
            34 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 26, 'm' => 35),
            35 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 26, 'a' => 36),
            36 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 26, 'r' => 37),
            37 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 26),
            38 => branch!('0'-'9', 'A'-'E', 'G'-'Z', '_', 'a'-'z' => 26, 'F' => 39),
            39 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 26),
            40 => branch!(),
            41 => branch!(),
            42 => branch!(),
            43 => branch!(),
            44 => branch!(),
        ],
        Some(0),
        btreemap![
            17 => term!(skip), 18 => term!(=1), 19 => term!(=5), 20 => term!(=8), 21 => term!(=3), 22 => term!(=7),
            23 => term!(=0), 24 => term!(=6), 25 => term!(=4), 26 => term!(=16), 27 => term!(=16), 28 => term!(=16),
            29 => term!(=2), 30 => term!(skip), 31 => term!(skip), 32 => term!(=16), 33 => term!(=16), 34 => term!(=16),
            35 => term!(=16), 36 => term!(=16), 37 => term!(=9), 38 => term!(=16), 39 => term!(=10), 40 => term!(=11),
            41 => term!(=12), 42 => term!(=13), 43 => term!(=14), 44 => term!(=15),
        ],
        Some(17),
    );

    // [gramlexer_stage_2]
    // -------------------------------------------------------------------------

    // - gets data from stage 1
    let dfa = Dfa::<Normalized>::build_from(dfa_tables);
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_terminals(TERMINALS);

    // - builds the lexer
    let mut lexgen = LexerGen::build_from(dfa);
    lexgen.symbol_table = Some(symbol_table);
    lexgen.set_lib_crate(LexigramCrate::Full);
    let src = lexgen.gen_source_code(indent);
    let mut log = lexgen.give_log();
    if EXPECTED_NBR_WARNINGS != log.num_warnings() {
        log.add_error(format!("Unexpected number of warnings: {} instead of {EXPECTED_NBR_WARNINGS}", log.num_warnings()));
        Err(log)
    } else {
        Ok((log, src))
    }
}

pub fn write_gramlexer_source() {
    let (log, result_src) = gramlexer_source(0, true)
        .inspect_err(|log| panic!("Failed to build lexer:\n{log}"))
        .unwrap();
    println!("Log:\n{log}");
    replace_tagged_source(GRAMLEXER_FILENAME, GRAMLEXER_TAG, &result_src)
        .expect("lexer source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::file_utils::get_tagged_source;
    use super::*;

    #[test]
    fn check_source() {
        const VERBOSE: bool = false;

        let (log, result_src) = gramlexer_source(0, VERBOSE)
            .inspect_err(|log| panic!("Failed to build lexer:\n{log}"))
            .unwrap();
        if !cfg!(miri) {
            if VERBOSE { println!("Log:\n{log}"); }
            let expected_src = get_tagged_source(GRAMLEXER_FILENAME, GRAMLEXER_TAG).unwrap_or(String::new());
            assert_eq!(result_src, expected_src);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_gramlexer_source();
    }
}
