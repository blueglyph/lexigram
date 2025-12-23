// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::file_utils::replace_tagged_source;
use lexigram_lib::dfa::{Dfa, DfaTables};
use lexigram_lib::{branch, btreemap, term, Normalized, SymbolTable};
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::build::BuildFrom;
use super::{LEXILEXER_FILENAME, LEXILEXER_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 33] = [
    ("Arrow",    Some("->")),       // 0
    ("Colon",    Some(":")),        // 1
    ("Comma",    Some(",")),        // 2
    ("Dot",      Some(".")),        // 3
    ("Ellipsis", Some("..")),       // 4
    ("Lbracket", Some("{")),        // 5
    ("Lparen",   Some("(")),        // 6
    ("Negate",   Some("~")),        // 7
    ("Minus",    Some("-")),        // 8
    ("Plus",     Some("+")),        // 9
    ("Or",       Some("|")),        // 10
    ("Question", Some("?")),        // 11
    ("Rbracket", Some("}")),        // 12
    ("Rparen",   Some(")")),        // 13
    ("Semicolon",Some(";")),        // 14
    ("Star",     Some("*")),        // 15
    ("Channels", Some("channels")), // 16
    ("Fragment", Some("fragment")), // 17
    ("Lexicon",  Some("lexicon")),  // 18
    ("Mode",     Some("mode")),     // 19
    ("Pop",      Some("pop")),      // 20
    ("Push",     Some("push")),     // 21
    ("More",     Some("more")),     // 22
    ("Skip",     Some("skip")),     // 23
    ("Type",     Some("type")),     // 24
    ("Channel",  Some("channel")),  // 25
    ("Id",       None),             // 26
    ("CharLit",  None),             // 27
    ("StrLit",   None),             // 28
    ("FixedSet", None),             // 29
    ("LSbracket",Some("[")),        // 30
    ("RSbracket",Some("]")),        // 31
    ("SetChar",  None),             // 32
];

// [terminal_symbols]
// -------------------------------------------------------------------------

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Lexi's lexer source code from the lexicon file.
fn lexilexer_source(indent: usize, _verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram-lib: 0.8.0
    // lexi-gram: 0.8.0
    // build-stage1: 0.8.0

    // [versions]

    // -------------------------------------------------------------------------
    // [lexilexer_stage_2]

    let dfa_tables = DfaTables::new(
        btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 21, '\'' => 1, '(' => 22, ')' => 23, '*' => 24, '+' => 25, ',' => 26, '-' => 27, '.' => 28, '/' => 2, ':' => 29, ';' => 30, '?' => 31, 'A'-'Z', 'a'-'b', 'd'-'e', 'g'-'k', 'n'-'o', 'q'-'r', 'u'-'z' => 32, '[' => 33, '\\' => 3, 'c' => 34, 'f' => 35, 'l' => 36, 'm' => 37, 'p' => 38, 's' => 39, 't' => 40, '{' => 41, '|' => 42, '}' => 43, '~' => 44),
            1 => branch!(~['\t'-'\n', '\r', '\'', '\\'] => 6, ['\\'] => 7),
            2 => branch!('*' => 4, '/' => 83),
            3 => branch!('d', 'w' => 87),
            4 => branch!(~['*'] => 4, ['*'] => 5),
            5 => branch!(~['*', '/'] => 4, ['*'] => 5, ['/'] => 84),
            6 => branch!(~['\t'-'\n', '\r', '\'', '\\'] => 11, ['\''] => 85, ['\\'] => 15),
            7 => branch!('\'', '\\', 'n', 'r', 't' => 6, 'u' => 8),
            8 => branch!('{' => 9),
            9 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 10),
            10 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 10, '}' => 6),
            11 => branch!(~['\t'-'\n', '\r', '\'', '\\'] => 11, ['\''] => 86, ['\\'] => 15),
            12 => branch!(~['\t'-'\n', '\r', '-', '\\'-']'] => 88, ['-'] => 89, ['\\'] => 13, [']'] => 90),
            13 => branch!('-', '['-']', 'n', 'r', 't' => 88, 'd', 'w' => 91, 'u' => 18),
            14 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 14, '}' => 88),
            15 => branch!('\'', '\\', 'n', 'r', 't' => 11, 'u' => 20),
            16 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 14),
            17 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 17, '}' => 11),
            18 => branch!('{' => 16),
            19 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 17),
            20 => branch!('{' => 19),
            21 => branch!('\t'-'\n', '\r', ' ' => 21),
            22 => branch!(),
            23 => branch!(),
            24 => branch!(),
            25 => branch!(),
            26 => branch!(),
            27 => branch!('>' => 45),
            28 => branch!('.' => 46),
            29 => branch!(),
            30 => branch!(),
            31 => branch!(),
            32 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            33 => branch!(),
            34 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'g', 'i'-'z' => 32, 'h' => 47),
            35 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 32, 'r' => 54),
            36 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 61),
            37 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'n', 'p'-'z' => 32, 'o' => 67),
            38 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'n', 'p'-'t', 'v'-'z' => 32, 'o' => 71, 'u' => 72),
            39 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'j', 'l'-'z' => 32, 'k' => 77),
            40 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'x', 'z' => 32, 'y' => 80),
            41 => branch!(),
            42 => branch!(),
            43 => branch!(),
            44 => branch!(),
            45 => branch!(),
            46 => branch!(),
            47 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 32, 'a' => 48),
            48 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'m', 'o'-'z' => 32, 'n' => 49),
            49 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'m', 'o'-'z' => 32, 'n' => 50),
            50 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 51),
            51 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'k', 'm'-'z' => 32, 'l' => 52),
            52 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'r', 't'-'z' => 32, 's' => 53),
            53 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            54 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 32, 'a' => 55),
            55 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'f', 'h'-'z' => 32, 'g' => 56),
            56 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 32, 'm' => 57),
            57 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 58),
            58 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'m', 'o'-'z' => 32, 'n' => 59),
            59 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'s', 'u'-'z' => 32, 't' => 60),
            60 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            61 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'w', 'y'-'z' => 32, 'x' => 62),
            62 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'h', 'j'-'z' => 32, 'i' => 63),
            63 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'b', 'd'-'z' => 32, 'c' => 64),
            64 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'n', 'p'-'z' => 32, 'o' => 65),
            65 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'m', 'o'-'z' => 32, 'n' => 66),
            66 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            67 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'c', 'e'-'q', 's'-'z' => 32, 'd' => 68, 'r' => 69),
            68 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 70),
            69 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 76),
            70 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            71 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'o', 'q'-'z' => 32, 'p' => 73),
            72 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'r', 't'-'z' => 32, 's' => 74),
            73 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            74 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'g', 'i'-'z' => 32, 'h' => 75),
            75 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            76 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            77 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'h', 'j'-'z' => 32, 'i' => 78),
            78 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'o', 'q'-'z' => 32, 'p' => 79),
            79 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            80 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'o', 'q'-'z' => 32, 'p' => 81),
            81 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 82),
            82 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            83 => branch!(~['\n', '\r'] => 83),
            84 => branch!(),
            85 => branch!(),
            86 => branch!(),
            87 => branch!(),
            88 => branch!(),
            89 => branch!(),
            90 => branch!(),
            91 => branch!(),
        ],
        Some(0),
        btreemap![
            21 => term!(skip), 22 => term!(=6), 23 => term!(=13), 24 => term!(=15), 25 => term!(=9), 26 => term!(=2),
            27 => term!(=8), 28 => term!(=3), 29 => term!(=1), 30 => term!(=14), 31 => term!(=11), 32 => term!(=26),
            33 => term!(=30) + term!(push 1) + term!(pushst 12), 34 => term!(=26), 35 => term!(=26), 36 => term!(=26), 37 => term!(=26), 38 => term!(=26),
            39 => term!(=26), 40 => term!(=26), 41 => term!(=5), 42 => term!(=10), 43 => term!(=12), 44 => term!(=7),
            45 => term!(=0), 46 => term!(=4), 47 => term!(=26), 48 => term!(=26), 49 => term!(=26), 50 => term!(=26),
            51 => term!(=26), 52 => term!(=25), 53 => term!(=16), 54 => term!(=26), 55 => term!(=26), 56 => term!(=26),
            57 => term!(=26), 58 => term!(=26), 59 => term!(=26), 60 => term!(=17), 61 => term!(=26), 62 => term!(=26),
            63 => term!(=26), 64 => term!(=26), 65 => term!(=26), 66 => term!(=18), 67 => term!(=26), 68 => term!(=26),
            69 => term!(=26), 70 => term!(=19), 71 => term!(=26), 72 => term!(=26), 73 => term!(=20), 74 => term!(=26),
            75 => term!(=21), 76 => term!(=22), 77 => term!(=26), 78 => term!(=26), 79 => term!(=23), 80 => term!(=26),
            81 => term!(=26), 82 => term!(=24), 83 => term!(skip), 84 => term!(skip), 85 => term!(=27), 86 => term!(=28),
            87 => term!(=29), 88 => term!(=32), 89 => term!(=8), 90 => term!(=31) + term!(pop), 91 => term!(=29),
        ],
        Some(21),
    );

    // [lexilexer_stage_2]
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

pub fn write_lexilexer() {
    let (log, result_src) = lexilexer_source(0, true)
        .inspect_err(|log| panic!("Failed to build lexer:\n{log}"))
        .unwrap();
    println!("Log:\n{log}");
    replace_tagged_source(LEXILEXER_FILENAME, LEXILEXER_TAG, &result_src)
        .expect("lexer source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::file_utils::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;

        let (log, result_src) = lexilexer_source(0, VERBOSE)
            .inspect_err(|log| panic!("Failed to build lexer:\n{log}"))
            .unwrap();
        if !cfg!(miri) {
            if VERBOSE { println!("Log:\n{log}"); }
            let expected_src = get_tagged_source(LEXILEXER_FILENAME, LEXILEXER_TAG).unwrap_or(String::new());
            assert_eq!(result_src, expected_src);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_lexilexer();
    }
}
