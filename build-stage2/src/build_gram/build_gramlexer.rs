// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::test_tools::replace_tagged_source;
use lexigram_lib::{branch, btreemap, term, Normalized, SymbolTable};
use lexigram_lib::dfa::{Dfa, DfaTables};
use lexigram_lib::log::{BufLog, BuildFrom, LogReader, LogStatus, Logger};
use super::{GRAMLEXER_FILENAME, GRAMLEXER_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 14] = [
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
    ("Id",       None),            // 13
];

// [terminal_symbols]
// -------------------------------------------------------------------------

const EXPECTED_NBR_WARNINGS: usize = 0;

fn gramlexer_source(indent: usize, _verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram_lib: 0.5.2
    // lexigram: 0.5.2
    // build-stage1: 0.5.2

    // [versions]

    // -------------------------------------------------------------------------
    // [gramlexer_stage_2]

    let dfa_tables = DfaTables::new(
        btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 10, '(' => 11, ')' => 12, '*' => 13, '+' => 14, '/' => 1, ':' => 15, ';' => 16, '<' => 2, '?' => 17, 'A'-'D', 'F'-'Z', 'a'-'f', 'h'-'z' => 18, 'E' => 19, 'g' => 20, '|' => 21),
            1 => branch!('*' => 3, '/' => 22),
            2 => branch!('L' => 5, 'P' => 6, 'R' => 7),
            3 => branch!(~['*'] => 3, ['*'] => 4),
            4 => branch!(~['*', '/'] => 3, ['*'] => 4, ['/'] => 23),
            5 => branch!('=' => 8, '>' => 32),
            6 => branch!('>' => 34),
            7 => branch!('>' => 33),
            8 => branch!('A'-'Z', 'a'-'z' => 9),
            9 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 9, '>' => 32),
            10 => branch!('\t'-'\n', '\r', ' ' => 10),
            11 => branch!(),
            12 => branch!(),
            13 => branch!(),
            14 => branch!(),
            15 => branch!(),
            16 => branch!(),
            17 => branch!(),
            18 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 18),
            19 => branch!('0'-'9', 'A'-'N', 'P'-'Z', '_', 'a'-'z' => 18, 'O' => 30),
            20 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 18, 'r' => 24),
            21 => branch!(),
            22 => branch!(~['\n', '\r'] => 22),
            23 => branch!(),
            24 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 18, 'a' => 25),
            25 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 18, 'm' => 26),
            26 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 18, 'm' => 27),
            27 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 18, 'a' => 28),
            28 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 18, 'r' => 29),
            29 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 18),
            30 => branch!('0'-'9', 'A'-'E', 'G'-'Z', '_', 'a'-'z' => 18, 'F' => 31),
            31 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 18),
            32 => branch!(),
            33 => branch!(),
            34 => branch!(),
        ],
        Some(0),
        btreemap![
            10 => term!(skip), 11 => term!(=1), 12 => term!(=5), 13 => term!(=7), 14 => term!(=3), 15 => term!(=0),
            16 => term!(=6), 17 => term!(=4), 18 => term!(=13), 19 => term!(=13), 20 => term!(=13), 21 => term!(=2),
            22 => term!(skip), 23 => term!(skip), 24 => term!(=13), 25 => term!(=13), 26 => term!(=13), 27 => term!(=13),
            28 => term!(=13), 29 => term!(=8), 30 => term!(=13), 31 => term!(=9), 32 => term!(=10), 33 => term!(=11),
            34 => term!(=12),
        ],
        Some(10),
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
    use lexigram_lib::test_tools::get_tagged_source;
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
