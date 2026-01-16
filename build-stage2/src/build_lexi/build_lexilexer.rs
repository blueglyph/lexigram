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

static TERMINALS: [(&str, Option<&str>); 34] = [
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
    ("Hook",     Some("hook")),     // 26
    ("Id",       None),             // 27
    ("CharLit",  None),             // 28
    ("StrLit",   None),             // 29
    ("FixedSet", None),             // 30
    ("LSbracket",Some("[")),        // 31
    ("RSbracket",Some("]")),        // 32
    ("SetChar",  None),             // 33
];

// [terminal_symbols]
// -------------------------------------------------------------------------

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Lexi's lexer source code from the lexicon file.
fn lexilexer_source(indent: usize, _verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram-lib: 0.8.3
    // lexi-gram: 0.8.3
    // build-stage1: 0.8.3

    // [versions]

    // -------------------------------------------------------------------------
    // [lexilexer_stage_2]

    let dfa_tables = DfaTables::new(
        btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 21, '\'' => 1, '(' => 22, ')' => 23, '*' => 24, '+' => 25, ',' => 26, '-' => 27, '.' => 28, '/' => 2, ':' => 29, ';' => 30, '?' => 31, 'A'-'Z', 'a'-'b', 'd'-'e', 'g', 'i'-'k', 'n'-'o', 'q'-'r', 'u'-'z' => 32, '[' => 33, '\\' => 3, 'c' => 34, 'f' => 35, 'h' => 36, 'l' => 37, 'm' => 38, 'p' => 39, 's' => 40, 't' => 41, '{' => 42, '|' => 43, '}' => 44, '~' => 45),
            1 => branch!(~['\t'-'\n', '\r', '\'', '\\'] => 6, ['\\'] => 7),
            2 => branch!('*' => 4, '/' => 87),
            3 => branch!('d', 'w' => 91),
            4 => branch!(~['*'] => 4, ['*'] => 5),
            5 => branch!(~['*', '/'] => 4, ['*'] => 5, ['/'] => 88),
            6 => branch!(~['\t'-'\n', '\r', '\'', '\\'] => 11, ['\''] => 89, ['\\'] => 15),
            7 => branch!('\'', '\\', 'n', 'r', 't' => 6, 'u' => 8),
            8 => branch!('{' => 9),
            9 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 10),
            10 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 10, '}' => 6),
            11 => branch!(~['\t'-'\n', '\r', '\'', '\\'] => 11, ['\''] => 90, ['\\'] => 15),
            12 => branch!(~['\t'-'\n', '\r', '-', '\\'-']'] => 92, ['-'] => 93, ['\\'] => 13, [']'] => 94),
            13 => branch!('-', '['-']', 'n', 'r', 't' => 92, 'd', 'w' => 95, 'u' => 18),
            14 => branch!('0'-'9', 'A'-'F', 'a'-'f' => 14, '}' => 92),
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
            27 => branch!('>' => 46),
            28 => branch!('.' => 47),
            29 => branch!(),
            30 => branch!(),
            31 => branch!(),
            32 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            33 => branch!(),
            34 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'g', 'i'-'z' => 32, 'h' => 48),
            35 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'q', 's'-'z' => 32, 'r' => 55),
            36 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'n', 'p'-'z' => 32, 'o' => 84),
            37 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 62),
            38 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'n', 'p'-'z' => 32, 'o' => 68),
            39 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'n', 'p'-'t', 'v'-'z' => 32, 'o' => 72, 'u' => 73),
            40 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'j', 'l'-'z' => 32, 'k' => 78),
            41 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'x', 'z' => 32, 'y' => 81),
            42 => branch!(),
            43 => branch!(),
            44 => branch!(),
            45 => branch!(),
            46 => branch!(),
            47 => branch!(),
            48 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 32, 'a' => 49),
            49 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'m', 'o'-'z' => 32, 'n' => 50),
            50 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'m', 'o'-'z' => 32, 'n' => 51),
            51 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 52),
            52 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'k', 'm'-'z' => 32, 'l' => 53),
            53 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'r', 't'-'z' => 32, 's' => 54),
            54 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            55 => branch!('0'-'9', 'A'-'Z', '_', 'b'-'z' => 32, 'a' => 56),
            56 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'f', 'h'-'z' => 32, 'g' => 57),
            57 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'l', 'n'-'z' => 32, 'm' => 58),
            58 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 59),
            59 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'m', 'o'-'z' => 32, 'n' => 60),
            60 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'s', 'u'-'z' => 32, 't' => 61),
            61 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            62 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'w', 'y'-'z' => 32, 'x' => 63),
            63 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'h', 'j'-'z' => 32, 'i' => 64),
            64 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'b', 'd'-'z' => 32, 'c' => 65),
            65 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'n', 'p'-'z' => 32, 'o' => 66),
            66 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'m', 'o'-'z' => 32, 'n' => 67),
            67 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            68 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'c', 'e'-'q', 's'-'z' => 32, 'd' => 69, 'r' => 70),
            69 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 71),
            70 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 77),
            71 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            72 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'o', 'q'-'z' => 32, 'p' => 74),
            73 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'r', 't'-'z' => 32, 's' => 75),
            74 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            75 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'g', 'i'-'z' => 32, 'h' => 76),
            76 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            77 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            78 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'h', 'j'-'z' => 32, 'i' => 79),
            79 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'o', 'q'-'z' => 32, 'p' => 80),
            80 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            81 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'o', 'q'-'z' => 32, 'p' => 82),
            82 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'d', 'f'-'z' => 32, 'e' => 83),
            83 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            84 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'n', 'p'-'z' => 32, 'o' => 85),
            85 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'j', 'l'-'z' => 32, 'k' => 86),
            86 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 32),
            87 => branch!(~['\n', '\r'] => 87),
            88 => branch!(),
            89 => branch!(),
            90 => branch!(),
            91 => branch!(),
            92 => branch!(),
            93 => branch!(),
            94 => branch!(),
            95 => branch!(),
        ],
        Some(0),
        btreemap![
            21 => term!(skip), 22 => term!(=6), 23 => term!(=13), 24 => term!(=15), 25 => term!(=9), 26 => term!(=2),
            27 => term!(=8), 28 => term!(=3), 29 => term!(=1), 30 => term!(=14), 31 => term!(=11), 32 => term!(=27),
            33 => term!(=31) + term!(push 1) + term!(pushst 12), 34 => term!(=27), 35 => term!(=27), 36 => term!(=27), 37 => term!(=27), 38 => term!(=27),
            39 => term!(=27), 40 => term!(=27), 41 => term!(=27), 42 => term!(=5), 43 => term!(=10), 44 => term!(=12),
            45 => term!(=7), 46 => term!(=0), 47 => term!(=4), 48 => term!(=27), 49 => term!(=27), 50 => term!(=27),
            51 => term!(=27), 52 => term!(=27), 53 => term!(=25), 54 => term!(=16), 55 => term!(=27), 56 => term!(=27),
            57 => term!(=27), 58 => term!(=27), 59 => term!(=27), 60 => term!(=27), 61 => term!(=17), 62 => term!(=27),
            63 => term!(=27), 64 => term!(=27), 65 => term!(=27), 66 => term!(=27), 67 => term!(=18), 68 => term!(=27),
            69 => term!(=27), 70 => term!(=27), 71 => term!(=19), 72 => term!(=27), 73 => term!(=27), 74 => term!(=20),
            75 => term!(=27), 76 => term!(=21), 77 => term!(=22), 78 => term!(=27), 79 => term!(=27), 80 => term!(=23),
            81 => term!(=27), 82 => term!(=27), 83 => term!(=24), 84 => term!(=27), 85 => term!(=27), 86 => term!(=26),
            87 => term!(skip), 88 => term!(skip), 89 => term!(=28), 90 => term!(=29), 91 => term!(=30), 92 => term!(=33),
            93 => term!(=8), 94 => term!(=32) + term!(pop), 95 => term!(=30),
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

pub fn write_lexilexer_source() {
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
    fn check_source() {
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
        write_lexilexer_source();
    }
}
