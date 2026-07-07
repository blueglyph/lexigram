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

static TERMINALS: [(&str, Option<&str>); 18] = [
    ("Colon",     Some(":")),         // 0
    ("Lparen",    Some("(")),         // 1
    ("Or",        Some("|")),         // 2
    ("Plus",      Some("+")),         // 3
    ("Question",  Some("?")),         // 4
    ("Rparen",    Some(")")),         // 5
    ("Semicolon", Some(";")),         // 6
    ("Sep",       Some("/")),         // 7
    ("Star",      Some("*")),         // 8
    ("StrLit",    None),              // 9
    ("Grammar",   Some("grammar")),   // 10
    ("SymEof",    Some("EOF")),       // 11
    ("Lform",     None),              // 12
    ("Rform",     Some("<R>")),       // 13
    ("Pform",     Some("<P>")),       // 14
    ("Greedy",    Some("<G>")),       // 15
    ("ResolveTag",Some("<resolve>")), // 16
    ("Id",        None),              // 17
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
            0 => branch!('\t'-'\n', '\r', ' ' => 23, '\'' => 1, '(' => 24, ')' => 25, '*' => 26, '+' => 27, '/' => 28, ':' => 29, ';' => 30, '<' => 2, '?' => 31, 'A'-'D', 'F'-'Z', 'a'-'f', 'h'-'z' => 32, 'E' => 33, 'g' => 34, '|' => 35),
            1 => branch!(~['\t'-'\n', '\r', '\'', '\\'] => 5, ['\\'] => 6),
            2 => branch!('G' => 10, 'L' => 11, 'P' => 12, 'R' => 13, 'r' => 14),
            3 => branch!(~['*'] => 3, ['*'] => 4),
            4 => branch!(~['*', '/'] => 3, ['*'] => 4, ['/'] => 37),
            5 => branch!(~['\t'-'\n', '\r', '\'', '\\'] => 5, ['\''] => 38, ['\\'] => 6),
            6 => branch!('\'', '\\', 'n', 'r', 't' => 5, 'u' => 7),
            7 => branch!('{' => 8),
            8 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 9),
            9 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 9, '}' => 5),
            10 => branch!('>' => 50),
            11 => branch!('=' => 15, '>' => 47),
            12 => branch!('>' => 49),
            13 => branch!('>' => 48),
            14 => branch!('e' => 17),
            15 => branch!('A'-'Z', 'a'-'z' => 16),
            16 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 16, '>' => 47),
            17 => branch!('s' => 18),
            18 => branch!('o' => 19),
            19 => branch!('l' => 20),
            20 => branch!('v' => 22),
            21 => branch!('>' => 51),
            22 => branch!('e' => 21),
            23 => branch!('\t'-'\n', '\r', ' ' => 23),
            24 => branch!(),
            25 => branch!(),
            26 => branch!(),
            27 => branch!(),
            28 => branch!('*' => 3, '/' => 36),
            29 => branch!(),
            30 => branch!(),
            31 => branch!(),
            32 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            33 => branch!('0'-'9', 'A'-'N', 'P'-'Z', '_', 'a'-'z' => 32, 'O' => 45),
            34 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 32, 'r' => 39),
            35 => branch!(),
            36 => branch!(~['\n', '\r'] => 36),
            37 => branch!(),
            38 => branch!(),
            39 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 32, 'a' => 40),
            40 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 32, 'm' => 41),
            41 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 32, 'm' => 42),
            42 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 32, 'a' => 43),
            43 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 32, 'r' => 44),
            44 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            45 => branch!('0'-'9', 'A'-'E', 'G'-'Z', '_', 'a'-'z' => 32, 'F' => 46),
            46 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            47 => branch!(),
            48 => branch!(),
            49 => branch!(),
            50 => branch!(),
            51 => branch!(),
        ],
        Some(0),
        btreemap![
            23 => term!(skip), 24 => term!(=1), 25 => term!(=5), 26 => term!(=8), 27 => term!(=3), 28 => term!(=7),
            29 => term!(=0), 30 => term!(=6), 31 => term!(=4), 32 => term!(=17), 33 => term!(=17), 34 => term!(=17),
            35 => term!(=2), 36 => term!(skip), 37 => term!(skip), 38 => term!(=9), 39 => term!(=17), 40 => term!(=17),
            41 => term!(=17), 42 => term!(=17), 43 => term!(=17), 44 => term!(=10), 45 => term!(=17), 46 => term!(=11),
            47 => term!(=12), 48 => term!(=13), 49 => term!(=14), 50 => term!(=15), 51 => term!(=16),
        ],
        Some(23),
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
