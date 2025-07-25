// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::test_tools::replace_tagged_source;
use lexigram_lib::{branch, btreemap, term, SymbolTable};
use lexigram_lib::dfa::DfaTables;
use super::{GRAMLEXER_FILENAME, GRAMLEXER_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 13] = [
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
    ("Id",       None),            // 12
];

// [terminal_symbols]
// -------------------------------------------------------------------------

fn gramlexer_source(indent: usize, _verbose: bool) -> String {
    // [versions]

    // lexigram_lib: 0.3.0
    // lexigram: 0.3.0
    // build-stage1: 0.3.0

    // [versions]

    // -------------------------------------------------------------------------
    // [gramlexer_stage_2]

    let dfa_tables = DfaTables::new(
        btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 9, '(' => 10, ')' => 11, '*' => 12, '+' => 13, '/' => 1, ':' => 14, ';' => 15, '<' => 2, '?' => 16, 'A'-'D', 'F'-'Z', 'a'-'f', 'h'-'z' => 17, 'E' => 18, 'g' => 19, '|' => 20),
            1 => branch!('*' => 3, '/' => 21),
            2 => branch!('L' => 5, 'R' => 6),
            3 => branch!(~['*'] => 3, ['*'] => 4),
            4 => branch!(~['*', '/'] => 3, ['*'] => 4, ['/'] => 22),
            5 => branch!('=' => 7, '>' => 31),
            6 => branch!('>' => 32),
            7 => branch!('A'-'Z', 'a'-'z' => 8),
            8 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 8, '>' => 31),
            9 => branch!('\t'-'\n', '\r', ' ' => 9),
            10 => branch!(),
            11 => branch!(),
            12 => branch!(),
            13 => branch!(),
            14 => branch!(),
            15 => branch!(),
            16 => branch!(),
            17 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 17),
            18 => branch!('0'-'9', 'A'-'N', 'P'-'Z', '_', 'a'-'z' => 17, 'O' => 29),
            19 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 17, 'r' => 23),
            20 => branch!(),
            21 => branch!(~['\n', '\r'] => 21),
            22 => branch!(),
            23 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 17, 'a' => 24),
            24 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 17, 'm' => 25),
            25 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 17, 'm' => 26),
            26 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 17, 'a' => 27),
            27 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 17, 'r' => 28),
            28 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 17),
            29 => branch!('0'-'9', 'A'-'E', 'G'-'Z', '_', 'a'-'z' => 17, 'F' => 30),
            30 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 17),
            31 => branch!(),
            32 => branch!(),
        ],
        Some(0),
        btreemap![
            9 => term!(skip), 10 => term!(=1), 11 => term!(=5), 12 => term!(=7), 13 => term!(=3), 14 => term!(=0),
            15 => term!(=6), 16 => term!(=4), 17 => term!(=12), 18 => term!(=12), 19 => term!(=12), 20 => term!(=2),
            21 => term!(skip), 22 => term!(skip), 23 => term!(=12), 24 => term!(=12), 25 => term!(=12), 26 => term!(=12),
            27 => term!(=12), 28 => term!(=8), 29 => term!(=12), 30 => term!(=9), 31 => term!(=10), 32 => term!(=11),
        ],
        Some(9),
    );

    // [gramlexer_stage_2]
    // -------------------------------------------------------------------------

    // - gets data from stage 1
    let dfa = dfa_tables.make_dfa();
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_terminals(TERMINALS);

    // - builds the lexer
    let mut lexgen = LexerGen::new();
    lexgen.build_from_dfa(dfa);
    lexgen.symbol_table = Some(symbol_table);
    lexgen.build_source_code(indent)
}

pub fn write_gramlexer() {
    let result_src = gramlexer_source(0, true);
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

        let _result_src = gramlexer_source(0, VERBOSE);
        if !cfg!(miri) {
            let expected_src = get_tagged_source(GRAMLEXER_FILENAME, GRAMLEXER_TAG).unwrap_or(String::new());
            assert_eq!(_result_src, expected_src);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_gramlexer();
    }
}
