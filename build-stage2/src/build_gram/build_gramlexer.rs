// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::file_utils::replace_tagged_source;
use lexigram_lib::{branch, btreemap, term, Normalized, SymbolTable};
use lexigram_lib::dfa::{Dfa, DfaTables};
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::build::BuildFrom;
use super::{GRAMLEXER_FILENAME, GRAMLEXER_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 15] = [
    ("Colon",    Some(":")),       // 0
    ("Lparen",   Some("(")),       // 1
    ("Or",       Some("|")),       // 2
    ("Plus",     Some("+")),       // 3
    ("Question", Some("?")),       // 4
    ("Rparen",   Some(")")),       // 5
    ("Semicolon",Some(";")),       // 6
    ("Star",     Some("*")),       // 7
    ("Grammar",  Some("grammar")), // 8
    ("SymEof",   Some("EOF")),     // 9
    ("Lform",    None),            // 10
    ("Rform",    Some("<R>")),     // 11
    ("Pform",    Some("<P>")),     // 12
    ("Greedy",   Some("<G>")),     // 13
    ("Id",       None),            // 14
];

// [terminal_symbols]
// -------------------------------------------------------------------------

const EXPECTED_NBR_WARNINGS: usize = 0;

fn gramlexer_source(indent: usize, _verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram-lib: 0.8.3
    // lexi-gram: 0.8.3
    // build-stage1: 0.8.3

    // [versions]

    // -------------------------------------------------------------------------
    // [gramlexer_stage_2]

    let dfa_tables = DfaTables::new(
        btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 11, '(' => 12, ')' => 13, '*' => 14, '+' => 15, '/' => 1, ':' => 16, ';' => 17, '<' => 2, '?' => 18, 'A'-'D', 'F'-'Z', 'a'-'f', 'h'-'z' => 19, 'E' => 20, 'g' => 21, '|' => 22),
            1 => branch!('*' => 3, '/' => 23),
            2 => branch!('G' => 5, 'L' => 6, 'P' => 7, 'R' => 8),
            3 => branch!(~['*'] => 3, ['*'] => 4),
            4 => branch!(~['*', '/'] => 3, ['*'] => 4, ['/'] => 24),
            5 => branch!('>' => 36),
            6 => branch!('=' => 9, '>' => 33),
            7 => branch!('>' => 35),
            8 => branch!('>' => 34),
            9 => branch!('A'-'Z', 'a'-'z' => 10),
            10 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 10, '>' => 33),
            11 => branch!('\t'-'\n', '\r', ' ' => 11),
            12 => branch!(),
            13 => branch!(),
            14 => branch!(),
            15 => branch!(),
            16 => branch!(),
            17 => branch!(),
            18 => branch!(),
            19 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 19),
            20 => branch!('0'-'9', 'A'-'N', 'P'-'Z', '_', 'a'-'z' => 19, 'O' => 31),
            21 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 19, 'r' => 25),
            22 => branch!(),
            23 => branch!(~['\n', '\r'] => 23),
            24 => branch!(),
            25 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 19, 'a' => 26),
            26 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 19, 'm' => 27),
            27 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 19, 'm' => 28),
            28 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 19, 'a' => 29),
            29 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 19, 'r' => 30),
            30 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 19),
            31 => branch!('0'-'9', 'A'-'E', 'G'-'Z', '_', 'a'-'z' => 19, 'F' => 32),
            32 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 19),
            33 => branch!(),
            34 => branch!(),
            35 => branch!(),
            36 => branch!(),
        ],
        Some(0),
        btreemap![
            11 => term!(skip), 12 => term!(=1), 13 => term!(=5), 14 => term!(=7), 15 => term!(=3), 16 => term!(=0),
            17 => term!(=6), 18 => term!(=4), 19 => term!(=14), 20 => term!(=14), 21 => term!(=14), 22 => term!(=2),
            23 => term!(skip), 24 => term!(skip), 25 => term!(=14), 26 => term!(=14), 27 => term!(=14), 28 => term!(=14),
            29 => term!(=14), 30 => term!(=8), 31 => term!(=14), 32 => term!(=9), 33 => term!(=10), 34 => term!(=11),
            35 => term!(=12), 36 => term!(=13),
        ],
        Some(11),
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
    lexgen.use_full_lib(true);
    let src = lexgen.gen_source_code(indent);
    let mut log = lexgen.give_log();
    if EXPECTED_NBR_WARNINGS != log.num_warnings() {
        log.add_error(format!("Unexpected number of warnings: {} instead of {EXPECTED_NBR_WARNINGS}", log.num_warnings()));
        Err(log)
    } else {
        Ok((log, src))
    }
}

pub fn write_gramlexer() {
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
    fn test_source() {
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
        write_gramlexer();
    }
}
