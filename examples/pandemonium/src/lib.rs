// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use std::collections::BTreeMap;
use lexigram_core::CollectJoin;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::{Parser, Terminate};
use lexigram_core::text_span::{GetLine, GetTextSpan};
use crate::listener_types::*;
use crate::pandemonium_lexer::build_lexer;
use crate::pandemonium_parser::*;

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

#[test]
fn test_pandemonium() {
    if VERBOSE { println!("{:=<80}\n{TXT1}\n{0:-<80}", ""); }
    let mut demo = PanDemo::new();
    match demo.parse(TXT1) {
        Ok(PanDemoResult { log, values, spans, rebuilt_txt }) => {
            let result_values = values.iter().map(|(id, v)| format!("[{id}][{v}]")).to_vec();
            if VERBOSE {
                println!("parsing successful\n{log}");
                println!("Values:\n{}\n", result_values.join("\n"));
                println!("Spans:\n{}", spans.join("\n"));
            }
            // checks that the values have been correctly captured from the context data:
            assert_eq!(result_values, VALUES1, "value mismatch");
            // checks that the text rebuilt from spans matches the original:
            assert!(TXT1.contains(&rebuilt_txt), "rebuilt text is wrong:\n{rebuilt_txt:?}");
            // checks the individual spans:
            // (tedious visual verification each time the test changes!)
            assert_eq!(
                spans, SPANS1, "span mismatch:\n{}",
                spans.iter().zip(SPANS1).enumerate()
                    .find_map(|(i, (left, right))| {
                        if left != right {
                            Some(format!("{i}:\t{left}\n\t{right}"))
                        } else {
                            None
                        }
                    })
                    .unwrap()
            );
        },
        Err(log) => panic!("errors during parsing:\n{log}"),
    }
}

/// Text to parse
static TXT1: &str = r#"
star    Alpha   = a, 101, 110, 150;
plus    Bravo   = 102, 120, 250;
l-star  Charlie = 103, then 130, then 350;
l-plus  Delta   = 104, 140, 450;
rrec    Echo    = 105, 150, 550;
l-rrec  Foxtrot = 106, 160, 650;
lrec    Golf    = 107, 170, 750;
amb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;

star-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];
plus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];
l-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];
l-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];

star    Mike     = x;
l-star  November = 202;
rrec    Oscar    = 203;
l-rrec  Papa     = 204;
lrec    Quebec   = 205;

sep-list     Romeo   = a:1, then b: 2, then c:3;
sep-list     Sierra  = d: 4;
sep-list-opt Tango   = e: 5, then f:6, then g: 7;
sep-list-opt Uniform =;
"#;

static VALUES1: &[&str] = &[
    "[Alpha][a*101*110*150]",
    "[Bravo][102+120+250]",
    "[Charlie][103,130,350]",
    "[Delta][104,140,450]",
    "[Foxtrot][106,160,650,<end>]",
    "[Kilo][2:Beta|Charlie|5:Echo]",
    "[Lima][21:Uniform||Victor||25:Yankee]",
    "[Mike][x]",
    "[November][202]",
    "[Papa][204,<end>]",
    "[Romeo][<a:1><b:2><c:3>]",
    "[Sierra][<d:4>]",
    "[Tango][<e/5><f/6><g/7>]",
    "[Uniform][-]",
];

/// Expected spans collected when parsing TXT1
static SPANS1: &[&str] = &[
    r#"exit_star("Alpha", "=", "a", ", 101, 110, 150", ";")"#,
    r#"exit_example("star", "Alpha   = a, 101, 110, 150;")"#,
    r#"exit_i("", "star    Alpha   = a, 101, 110, 150;")"#,
    r#"exit_plus("Bravo", "=", "102", ", 120, 250", ";")"#,
    r#"exit_example("plus", "Bravo   = 102, 120, 250;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;", "plus    Bravo   = 102, 120, 250;")"#,
    r#"exit_l_star_i("103", ",", "then", "130")"#,
    r#"exit_l_star_i("103, then 130", ",", "then", "350")"#,
    r#"exit_l_star("Charlie", "=", "103, then 130, then 350", ";")"#,
    r#"exit_example("l-star", "Charlie = 103, then 130, then 350;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;", "l-star  Charlie = 103, then 130, then 350;")"#,
    r#"exit_l_plus_i("", ",", "140")"#,
    r#"exit_l_plus_i(", 140", ",", "450")"#,
    r#"exit_l_plus("Delta", "=", "104", ", 140, 450", ";")"#,
    r#"exit_example("l-plus", "Delta   = 104, 140, 450;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;", "l-plus  Delta   = 104, 140, 450;")"#,
    r#"exit_rrec_i(";")"#,
    r#"exit_rrec_i(",", "550", ";")"#,
    r#"exit_rrec_i(",", "150", ", 550;")"#,
    r#"exit_rrec("Echo", "=", "105", ", 150, 550;")"#,
    r#"exit_example("rrec", "Echo    = 105, 150, 550;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;", "rrec    Echo    = 105, 150, 550;")"#,
    r#"exit_l_rrec_i("", ",", "160")"#,
    r#"exit_l_rrec_i(", 160", ",", "650")"#,
    r#"exit_l_rrec_i(", 160, 650", ";")"#,
    r#"exit_l_rrec("Foxtrot", "=", "106", ", 160, 650;")"#,
    r#"exit_example("l-rrec", "Foxtrot = 106, 160, 650;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;", "l-rrec  Foxtrot = 106, 160, 650;")"#,
    r#"exit_lrec_i("107")"#,
    r#"exit_lrec_i("107", ",", "170")"#,
    r#"exit_lrec_i("107, 170", ",", "750")"#,
    r#"exit_lrec("Golf", "=", "107, 170, 750", ";")"#,
    r#"exit_example("lrec", "Golf    = 107, 170, 750;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;", "lrec    Golf    = 107, 170, 750;")"#,
    r#"exit_amb_i("5")"#,
    r#"exit_amb_i("2")"#,
    r#"exit_amb_i("6")"#,
    r#"exit_amb_i("3")"#,
    r#"exit_amb_i("2")"#,
    r#"exit_amb_i("4")"#,
    r#"exit_amb_i("2", "^", "4")"#,
    r#"exit_amb_i("3", "^", "2^4")"#,
    r#"exit_amb_i("81")"#,
    r#"exit_amb_i("3^2^4", "/", "81")"#,
    r#"exit_amb_i("6", "+", "3^2^4 / 81")"#,
    r#"exit_amb_i("-", "6 + 3^2^4 / 81")"#,
    r#"exit_amb_i("2", "*", "-6 + 3^2^4 / 81")"#,
    r#"exit_amb_i("5", "-", "2*-6 + 3^2^4 / 81")"#,
    r#"exit_amb("Hotel", "=", "5 - 2*-6 + 3^2^4 / 81", ";")"#,
    r#"exit_example("amb", "Hotel   = 5 - 2*-6 + 3^2^4 / 81;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;", "amb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;")"#,

    r#"exit_star_a("India", "=", "[", "1:Alpha Beta 4:Delta Echo 10:Juliet", "]", ";")"#,
    r#"exit_example("star-a", "India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;", "star-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];")"#,
    r#"exit_plus_a("Juliet", "=", "[", "11:Kilo Lima Mike 26:Zoulou", "]", ";")"#,
    r#"exit_example("plus-a", "Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];", "plus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];")"#,
    r#"exit_l_star_a_i("", "2", ":", "Beta")"#,
    r#"exit_l_star_a_i("2:Beta", "Charlie")"#,
    r#"exit_l_star_a_i("2:Beta Charlie", "5", ":", "Echo")"#,
    r#"exit_l_star_a("Kilo", "=", "[", "2:Beta Charlie 5:Echo", "]", ";")"#,
    r#"exit_example("l-star-a", "Kilo   = [ 2:Beta Charlie 5:Echo ];")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];", "l-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];")"#,
    r#"exit_l_plus_a_i("", "21", ":", "Uniform")"#,
    r#"exit_l_plus_a_i("21:Uniform", "Victor")"#,
    r#"exit_l_plus_a_i("21:Uniform Victor", "25", ":", "Yankee")"#,
    r#"exit_l_plus_a("Lima", "=", "[", "21:Uniform Victor 25:Yankee", "]", ";")"#,
    r#"exit_example("l-plus-a", "Lima   = [ 21:Uniform Victor 25:Yankee ];")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];", "l-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];")"#,

    r#"exit_star("Mike", "=", "x", "", ";")"#,
    r#"exit_example("star", "Mike     = x;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];", "star    Mike     = x;")"#,
    r#"exit_l_star("November", "=", "202", ";")"#,
    r#"exit_example("l-star", "November = 202;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;", "l-star  November = 202;")"#,
    r#"exit_rrec_i(";")"#,
    r#"exit_rrec("Oscar", "=", "203", ";")"#,
    r#"exit_example("rrec", "Oscar    = 203;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;", "rrec    Oscar    = 203;")"#,
    r#"exit_l_rrec_i("", ";")"#,
    r#"exit_l_rrec("Papa", "=", "204", ";")"#,
    r#"exit_example("l-rrec", "Papa     = 204;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;", "l-rrec  Papa     = 204;")"#,
    r#"exit_lrec_i("205")"#,
    r#"exit_lrec("Quebec", "=", "205", ";")"#,
    r#"exit_example("lrec", "Quebec   = 205;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nl-rrec  Papa     = 204;", "lrec    Quebec   = 205;")"#,

    r#"exit_sep_list("Romeo", "=", "a:1, then b: 2, then c:3", ";")"#,
    r#"exit_example("sep-list", "Romeo   = a:1, then b: 2, then c:3;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nl-rrec  Papa     = 204;\nlrec    Quebec   = 205;", "sep-list     Romeo   = a:1, then b: 2, then c:3;")"#,
    r#"exit_sep_list("Sierra", "=", "d: 4", ";")"#,
    r#"exit_example("sep-list", "Sierra  = d: 4;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nl-rrec  Papa     = 204;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;", "sep-list     Sierra  = d: 4;")"#,
    r#"exit_sep_list_opt("Tango", "=", "e: 5, then f:6, then g: 7", ";")"#,
    r#"exit_example("sep-list-opt", "Tango   = e: 5, then f:6, then g: 7;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nl-rrec  Papa     = 204;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;", "sep-list-opt Tango   = e: 5, then f:6, then g: 7;")"#,
    r#"exit_sep_list_opt("Uniform", "=", ";")"#,
    r#"exit_example("sep-list-opt", "Uniform =;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nl-rrec  Papa     = 204;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;\nsep-list-opt Tango   = e: 5, then f:6, then g: 7;", "sep-list-opt Uniform =;")"#,

    r#"exit_text("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, then 130, then 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nl-rrec  Papa     = 204;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;\nsep-list-opt Tango   = e: 5, then f:6, then g: 7;\nsep-list-opt Uniform =;")"#,
];

// -------------------------------------------------------------------------
// minimalist parser, top level

pub struct PanDemo<'l, 'p, 'ls> {
    lexer: Lexer<'l, &'ls [u8]>,
    parser: Parser<'p>,
    wrapper: Wrapper<PanDemoListener<'ls>>,
}

impl<'l, 'ls: 'l> PanDemo<'l, '_, 'ls> {
    fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        let wrapper = Wrapper::new(PanDemoListener::new(), VERBOSE_WRAPPER);
        PanDemo { lexer, parser, wrapper }
    }

    fn parse(&'ls mut self, text: &'ls str) -> Result<PanDemoResult, BufLog> {
        let stream = CharReader::new(text.as_bytes());
        self.lexer.attach_stream(stream);
        self.wrapper.get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(&mut self.wrapper, tokens) {
            self.wrapper.get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let log = std::mem::take(&mut self.wrapper.get_listener_mut().log);
        if log.has_no_errors() {
            let listener = self.wrapper.get_listener_mut();
            let values = std::mem::take(&mut listener.values);
            let spans = std::mem::take(&mut listener.spans);
            let rebuilt_txt = listener.rebuilt_txt.take().unwrap();
            Ok(PanDemoResult { log, values, spans, rebuilt_txt })
        } else {
            Err(log)
        }
    }
}

// listener implementation

struct PanDemoResult {
    log: BufLog,
    values: BTreeMap<String, String>,
    spans: Vec<String>,
    rebuilt_txt: String,
}

struct PanDemoListener<'ls> {
    log: BufLog,
    abort: Terminate,
    spans: Vec<String>,
    lines: Option<Vec<&'ls str>>,
    rebuilt_txt: Option<String>,
    values: BTreeMap<String, String>,
}

impl<'ls> PanDemoListener<'ls> {
    fn new() -> Self {
        PanDemoListener {
            log: BufLog::new(),
            abort: Terminate::None,
            spans: vec![],
            lines: None,
            rebuilt_txt: None,
            values: BTreeMap::new(),
        }
    }

    fn attach_lines(&mut self, lines: Vec<&'ls str>) {
        self.lines = Some(lines);
    }

    fn add_value(&mut self, id: String, value: String) {
        if let Some(old) = self.values.insert(id.clone(), value) {
            let new = self.values.get(&id).unwrap();
            panic!("{}", format!("key was already in the values:\n- before: {id} = {old}\n- now   : {id} = {new}"));
        };
    }
}

impl GetLine for PanDemoListener<'_> {
    fn get_line(&self, n: usize) -> &str {
        self.lines.as_ref().unwrap()[n - 1]
    }
}

#[allow(unused)]
impl PandemoniumListener for PanDemoListener<'_> {
    fn check_abort_request(&self) -> Terminate {
        self.abort
    }

    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit(&mut self, text: SynText, span: PosSpan) {
        self.rebuilt_txt = Some(self.extract_text(&span));
    }

    fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) -> SynText {
        self.spans.push(format!("exit_text({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxText::V1 { star } => {} // text -> (<L> example)*
        }
        SynText()
    }

    fn init_i(&mut self) -> SynI {
        SynI()
    }

    fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxI::V1 { example: SynExample() } => {}
        }
    }

    fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) -> SynExample {
        self.spans.push(format!("exit_example({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxExample::V1 { star: SynStar() } => {}
            CtxExample::V2 { plus: SynPlus() } => {}
            CtxExample::V3 { l_star: SynLStar() } => {}
            CtxExample::V4 { l_plus: SynLPlus() } => {}
            CtxExample::V5 { rrec: SynRrec() } => {}
            CtxExample::V6 { l_rrec: SynLRrec() } => {}
            CtxExample::V7 { lrec: SynLrec() } => {}
            CtxExample::V8 { amb: SynAmb() } => {}
            CtxExample::V9 { star_a: SynStarA() } => {}
            CtxExample::V10 { plus_a: SynPlusA() } => {}
            CtxExample::V11 { l_star_a: SynLStarA() } => {}
            CtxExample::V12 { l_plus_a: SynLPlusA() } => {}
            CtxExample::V13 { sep_list: SynSepList() } => {}
            CtxExample::V14 { sep_list_opt: SynSepListOpt() } => {}
        }
        SynExample()
    }

    fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar {
        self.spans.push(format!("exit_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxStar::V1 { id: [id0, id1], star: SynStar1(star) } => {
                self.add_value(id0, format!("{id1}{}", star.into_iter().map(|s| format!("*{s}")).join("")));
            }
        }
        SynStar()
    }

    fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) -> SynPlus {
        self.spans.push(format!("exit_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxPlus::V1 { id, num, plus: SynPlus1(plus) } = ctx;
        self.add_value(id, format!("{num}+{}", plus.join("+")));
        SynPlus()
    }

    fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) -> SynLStar {
        self.spans.push(format!("exit_l_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLStar::V1 { id, star: SynLStarI(items) } = ctx;
        self.add_value(id, items.join(","));
        SynLStar()
    }

    fn init_l_star_i(&mut self, ctx: InitCtxLStarI, spans: Vec<PosSpan>) -> SynLStarI {
        let InitCtxLStarI::V1 { num } = ctx;
        assert_eq!(num, self.extract_text(&spans[0]));
        SynLStarI(vec![num])
    }

    fn exit_l_star_i(&mut self, acc: &mut SynLStarI, ctx: CtxLStarI, spans: Vec<PosSpan>) {
        // `<L> "," "then" Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," "then" Num ◄◄ )* ";"`
        let CtxLStarI::V1 { num } = ctx;
        assert_eq!(num, self.extract_text(&spans[3]));
        self.spans.push(format!("exit_l_star_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        acc.0.push(num);
    }

    fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) -> SynLPlus {
        self.spans.push(format!("exit_l_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLPlus::V1 { id, num, plus: SynLPlusI(items) } = ctx;
        self.add_value(id, format!("{num},{}", items.join(",")));
        SynLPlus()
    }

    fn init_l_plus_i(&mut self) -> SynLPlusI {
        SynLPlusI(vec![])
    }

    fn exit_l_plus_i(&mut self, acc: &mut SynLPlusI, ctx: CtxLPlusI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLPlusI::V1 { num, last_iteration } = ctx;
        acc.0.push(num);
    }

    fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) -> SynRrec {
        self.spans.push(format!("exit_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxRrec::V1 { id, num, rrec_i: SynRrecI() } = ctx;
        SynRrec()
    }

    fn exit_l_rrec(&mut self, ctx: CtxLRrec, spans: Vec<PosSpan>) -> SynLRrec {
        self.spans.push(format!("exit_l_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLRrec::V1 { id, num, l_rrec_i: SynLRrecI(mut list) } = ctx;
        list.insert(0, num);
        self.add_value(id, list.join(","));
        SynLRrec()
    }

    fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) -> SynLrec {
        self.spans.push(format!("exit_lrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxLrec::V1 { id, lrec_i: SynLrecI() } => {}
        }
        SynLrec()
    }

    fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) -> SynAmb {
        self.spans.push(format!("exit_amb({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxAmb::V1 { id, amb_i: SynAmbI() } => {}
        }
        SynAmb()
    }

    fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) -> SynStarA {
        self.spans.push(format!("exit_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // star_a -> Id "=" "[" (Id | Num ":" Id)* "]" ";"
            CtxStarA::V1 { id, star: SynStarA1(items) } => {}
        }
        SynStarA()
    }

    fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) -> SynPlusA {
        self.spans.push(format!("exit_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // plus_a -> Id "=" "[" (Id | Num ":" Id)+ "]" ";"
            CtxPlusA::V1 { id, plus: SynPlusA1(items) } => {}
        }
        SynPlusA()
    }

    fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) -> SynLStarA {
        self.spans.push(format!("exit_l_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_star_a -> Id "=" "[" (<L> Id | Num ":" Id)* "]" ";"
        let CtxLStarA::V1 { id, star: SynLStarAI(items) } = ctx;
        self.add_value(id, items.join("|"));
        SynLStarA()
    }

    fn init_l_star_a_i(&mut self) -> SynLStarAI {
        SynLStarAI(vec![])
    }

    fn exit_l_star_a_i(&mut self, acc: &mut SynLStarAI, ctx: CtxLStarAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let value = match ctx {
            // `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
            CtxLStarAI::V1 { id } => id,
            // `Num ":" Id` iteration in `l_star_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )* "]" ";"`
            CtxLStarAI::V2 { num, id } => format!("{num}:{id}"),
        };
        acc.0.push(value);
    }

    fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) -> SynLPlusA {
        self.spans.push(format!("exit_l_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_plus_a -> Id "=" "[" (<L> Id | Num ":" Id)+ "]" ";"
        let CtxLPlusA::V1 { id, plus: SynLPlusAI(items) } = ctx;
        self.add_value(id, items.join("||"));
        SynLPlusA()
    }

    fn init_l_plus_a_i(&mut self) -> SynLPlusAI {
        SynLPlusAI(vec![])
    }

    fn exit_l_plus_a_i(&mut self, acc: &mut SynLPlusAI, ctx: CtxLPlusAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let value = match ctx {
            // `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
            CtxLPlusAI::V1 { id, last_iteration } => id,
            // `Num ":" Id` iteration in `l_plus_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
            CtxLPlusAI::V2 { num, id, last_iteration } => format!("{num}:{id}"),
        };
        acc.0.push(value);
    }

    fn exit_sep_list(&mut self, ctx: CtxSepList, spans: Vec<PosSpan>) -> SynSepList {
        self.spans.push(format!("exit_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // sep_list -> Id "=" Id ":" Num ("," Id ":" Num)* ";"
        let CtxSepList::V1 { id, star: SynSepList1(items) } = ctx;
        let value = items.into_iter().map(|SynSepList1Item { id, num }| format!("<{id}:{num}>")).join("");
        self.add_value(id, value);
        SynSepList()
    }

    fn exit_sep_list_opt(&mut self, ctx: CtxSepListOpt, spans: Vec<PosSpan>) -> SynSepListOpt {
        self.spans.push(format!("exit_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let (id, value) = match ctx {
            // `sep_list_opt -> Id "=" Id ":" Num ("," Id ":" Num)* ";"`
            CtxSepListOpt::V1 { id, star: SynSepListOpt1(items) } =>
                (id, items.into_iter().map(|SynSepListOpt1Item { id, num }| format!("<{id}/{num}>")).join("")),
            // `sep_list_opt -> Id "=" ";"`
            CtxSepListOpt::V2 { id } =>
                (id, "-".to_string()),
        };
        self.add_value(id, value);
        SynSepListOpt()
    }

    fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) -> SynRrecI {
        self.spans.push(format!("exit_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxRrecI::V1 { num, rrec_i: SynRrecI() } => {}
            CtxRrecI::V2 => {}
        }
        SynRrecI()
    }

    fn init_l_rrec_i(&mut self) -> SynLRrecI {
        SynLRrecI(vec![])
    }

    fn exit_l_rrec_i(&mut self, acc: &mut SynLRrecI, ctx: CtxLRrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxLRrecI::V1 { num } => {
                acc.0.push(num);
            }
            CtxLRrecI::V2 => {
                acc.0.push("<end>".to_string());
            }
        }
    }

    fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) -> SynLrecI {
        self.spans.push(format!("exit_lrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxLrecI::V1 { lrec_i: SynLrecI(), num } => {}
            CtxLrecI::V2 { num } => {}
        }
        SynLrecI()
    }

    fn exitloop_lrec_i(&mut self, _lrec_i: &mut SynLrecI) {
    }

    fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) -> SynAmbI {
        self.spans.push(format!("exit_amb_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // `amb_i -> <R> amb_i "^" amb_i`
            CtxAmbI::V1 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> amb_i "*" amb_i`
            CtxAmbI::V2 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> amb_i <P> "/" amb_i`
            CtxAmbI::V3 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> amb_i "+" amb_i`
            CtxAmbI::V4 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> amb_i <P> "-" amb_i`
            CtxAmbI::V5 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> "-" amb_i`
            CtxAmbI::V6 { amb_i: SynAmbI() } => {}
            // `amb_i -> "(" amb_i ")"`
            CtxAmbI::V7 { amb_i: SynAmbI() } => {}
            // `amb_i -> Id`
            CtxAmbI::V8 { id } => {}
            // `amb_i -> Num`
            CtxAmbI::V9 { num } => {}
        }
        SynAmbI()
    }
}

// -------------------------------------------------------------------------
// User types used in the listener interface:
// (initially copied/uncommented from the generated parser code)

pub mod listener_types {
    /// User-defined type for `text`
    #[derive(Debug, PartialEq)] pub struct SynText();
    /// User-defined type for `<L> example` iteration in `text -> ( ►► <L> example ◄◄ )*`
    #[derive(Debug, PartialEq)] pub struct SynI();
    /// User-defined type for `example`
    #[derive(Debug, PartialEq)] pub struct SynExample();
    /// User-defined type for `star`
    #[derive(Debug, PartialEq)] pub struct SynStar();
    /// User-defined type for `plus`
    #[derive(Debug, PartialEq)] pub struct SynPlus();
    /// User-defined type for `l_star`
    #[derive(Debug, PartialEq)] pub struct SynLStar();
    /// User-defined type for `<L> "," Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," Num ◄◄ )* ";"`
    #[derive(Debug, PartialEq)] pub struct SynLStarI(pub Vec<String>);
    /// User-defined type for `l_plus`
    #[derive(Debug, PartialEq)] pub struct SynLPlus();
    /// User-defined type for `<L> "," Num` iteration in `l_plus -> Id "=" Num ( ►► <L> "," Num ◄◄ )+ ";"`
    #[derive(Debug, PartialEq)] pub struct SynLPlusI(pub Vec<String>);
    /// User-defined type for `rrec`
    #[derive(Debug, PartialEq)] pub struct SynRrec();
    /// User-defined type for `l_rrec`
    #[derive(Debug, PartialEq)] pub struct SynLRrec();
    /// User-defined type for `lrec`
    #[derive(Debug, PartialEq)] pub struct SynLrec();
    /// User-defined type for `amb`
    #[derive(Debug, PartialEq)] pub struct SynAmb();
    /// User-defined type for `star_a`
    #[derive(Debug, PartialEq)] pub struct SynStarA();
    /// User-defined type for `plus_a`
    #[derive(Debug, PartialEq)] pub struct SynPlusA();
    /// User-defined type for `l_star_a`
    #[derive(Debug, PartialEq)] pub struct SynLStarA();
    /// User-defined type for `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
    #[derive(Debug, PartialEq)] pub struct SynLStarAI(pub Vec<String>);
    /// User-defined type for `l_plus_a`
    #[derive(Debug, PartialEq)] pub struct SynLPlusA();
    /// User-defined type for `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
    #[derive(Debug, PartialEq)] pub struct SynLPlusAI(pub Vec<String>);
    /// User-defined type for `sep_list`
    #[derive(Debug, PartialEq)] pub struct SynSepList();
    /// User-defined type for `sep_list_opt`
    #[derive(Debug, PartialEq)] pub struct SynSepListOpt();
    /// User-defined type for `rrec_i`
    #[derive(Debug, PartialEq)] pub struct SynRrecI();
    /// User-defined type for `l_rrec_i`
    #[derive(Debug, PartialEq)] pub struct SynLRrecI(pub Vec<String>);
    /// User-defined type for `lrec_i`
    #[derive(Debug, PartialEq)] pub struct SynLrecI();
    /// User-defined type for `amb_i`
    #[derive(Debug, PartialEq)] pub struct SynAmbI();
}

// -------------------------------------------------------------------------

pub mod pandemonium_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [pandemonium_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 35;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 24;
    const NBR_STATES: StateId = 76;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         29,  29,  29,  29,  29,  29,  29,  29,  29,   0,  32,  29,  29,  32,  29,  29,   // 0-15
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,   // 16-31
          0,  29,  29,  29,  29,  29,  29,  29,   1,   2,   3,   4,   5,   6,  29,   7,   // 32-47
         22,   8,   8,   8,   8,   8,   8,   8,   8,   8,   9,  10,  29,  11,  29,  29,   // 48-63
         29,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,   // 64-79
         27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  13,  29,  14,  15,  28,   // 80-95
         29,  16,  34,  24,  27,  23,  27,  27,  31,  25,  27,  27,  17,  30,  33,  26,   // 96-111
         18,  27,  19,  20,  21,  12,  27,  27,  27,  27,  27,  29,  29,  29,  29,  29,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 29),
        (Seg(57344, 1114111), 29),
    ];
    static TERMINAL_TABLE: [Terminal;52] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 2661] = [
         24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  76,  36,  36,  36,  36,  36,  76,  76,  36,  36,  24,  36,  36, // state 0
          1,   1,   1,  22,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 1
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  68,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 2
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  69,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 3
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,   5,   6,   7,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 4
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  11,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 5
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  14,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 6
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,   8,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 7
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,   9,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 8
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  58,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 9
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  70,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 10
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  12,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 11
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  59,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 12
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  71,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 13
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  15,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 14
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  60,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 15
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  17,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 16
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  18,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 17
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  73,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 18
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  20,  76,  76,  76,  76,  76,  76,  76,  76, // state 19
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  21,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 20
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  74,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 21
          1,   1,   1,  22,   1,   1,   1,  75,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 22
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  16,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 23
         24,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  24,  76,  76, // state 24 <skip>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 25 <end:4>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 26 <end:7>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 27 <end:6>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 28 <end:0>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 29 <end:11>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 30 <end:9>
         76,  76,  76,   1,  76,  76,  76,  46,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 31 <end:1>
         76,  76,  76,  76,  76,  76,  76,  76,  32,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  32,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 32 <end:29>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 33 <end:10>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 34 <end:12>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 35 <end:2>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 36 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 37 <end:5>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 38 <end:8>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 39 <end:3>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  66,  36,  76,  36,  36, // state 40 <end:28>
         76,  76,  76,  76,  76,  76,   4,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  57,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 41 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  54,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 42 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  61,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 43 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  51,  36,  50,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 44 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  47,  76,  36,  36, // state 45 <end:28>
         46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  76,  46,  46, // state 46 <skip>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  48,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 47 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  49,  36, // state 48 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 49 <end:13>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  72,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 50 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  52,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 51 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  53,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 52 <end:28>
         76,  76,  76,  76,  76,  76,   2,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 53 <end:14>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  55,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 54 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  56,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 55 <end:28>
         76,  76,  76,  76,  76,  76,   3,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 56 <end:15>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  64,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 57 <end:28>
         76,  76,  76,  76,  76,  76,  10,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 58 <end:16>
         76,  76,  76,  76,  76,  76,  13,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 59 <end:17>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 60 <end:18>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  62,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 61 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  63,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 62 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 63 <end:19>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  65,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 64 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 65 <end:20>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  67, // state 66 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 67 <end:21>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 68 <end:22>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 69 <end:23>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 70 <end:24>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 71 <end:25>
         76,  76,  76,  76,  76,  76,  23,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 72 <end:28>
         76,  76,  76,  76,  76,  76,  19,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 73 <end:26>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 74 <end:27>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 75 <skip>
         76 // error group in [nbr_state * nbr_group + nbr_group]
    ];

    pub fn build_lexer<R: Read>() -> Lexer<'static, R> {
        Lexer::new(
            // parameters
            NBR_GROUPS,
            INITIAL_STATE,
            FIRST_END_STATE,
            NBR_STATES,
            // tables
            &ASCII_TO_GROUP,
            HashMap::<char, GroupId>::from(UTF8_TO_GROUP),
            SegMap::<GroupId>::from(SEG_TO_GROUP),
            &STATE_TABLE,
            &TERMINAL_TABLE,
        )
    }

    // [pandemonium_lexer]
}

// -------------------------------------------------------------------------

pub mod pandemonium_parser {
    // Generated code, don't modify manually anything between the tags below

    // [pandemonium_parser]

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser, Terminate}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 30;
    const PARSER_NUM_NT: usize = 45;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Add", Some("+")), ("Div", Some("/")), ("Equal", Some("=")), ("Exp", Some("^")), ("Lpar", Some("(")), ("Lsbracket", Some("[")), ("Mul", Some("*")), ("Rpar", Some(")")), ("Rsbracket", Some("]")), ("Sub", Some("-")), ("Colon", Some(":")), ("Comma", Some(",")), ("Semi", Some(";")), ("Then", Some("then")), ("Star", Some("star")), ("Plus", Some("plus")), ("L_Star", Some("l-star")), ("L_Plus", Some("l-plus")), ("L_Rrec", Some("l-rrec")), ("Rrec", Some("rrec")), ("Lrec", Some("lrec")), ("Amb", Some("amb")), ("Star_A", Some("star-a")), ("Plus_A", Some("plus-a")), ("L_Star_A", Some("l-star-a")), ("L_Plus_A", Some("l-plus-a")), ("SepList", Some("sep-list")), ("SepList_Opt", Some("sep-list-opt")), ("Id", None), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["text", "i", "example", "star", "plus", "l_star", "l_star_i", "l_plus", "l_plus_i", "rrec", "l_rrec", "lrec", "amb", "star_a", "plus_a", "l_star_a", "l_star_a_i", "l_plus_a", "l_plus_a_i", "sep_list", "sep_list_opt", "rrec_i", "l_rrec_i", "lrec_i", "amb_i", "star_1", "plus_1", "star_a_1", "plus_a_1", "sep_list_1", "sep_list_opt_1", "lrec_i_1", "amb_i_1", "amb_i_2", "amb_i_3", "amb_i_4", "amb_i_5", "amb_i_6", "l_plus_i_1", "l_plus_a_i_1", "l_plus_a_i_2", "sep_list_opt_2", "plus_2", "plus_a_2", "plus_a_3"];
    static ALT_VAR: [VarId; 91] = [0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 16, 16, 17, 18, 18, 19, 20, 21, 21, 22, 22, 23, 24, 25, 25, 26, 27, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32, 32, 32, 32, 32, 33, 34, 34, 34, 34, 35, 36, 36, 37, 37, 37, 37, 38, 38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44];
    static PARSING_TABLE: [AltId; 1395] = [91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 91, 91, 0, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 91, 91, 2, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 3, 4, 5, 6, 8, 7, 9, 10, 11, 12, 13, 14, 15, 16, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 17, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 18, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 19, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 20, 21, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 22, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 23, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 24, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 25, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 26, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 27, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 28, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 29, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 30, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 33, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 31, 32, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 34, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 35, 36, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 37, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 38, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 39, 40, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 41, 42, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 43, 91, 92, 92, 91, 92, 44, 91, 92, 92, 91, 44, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 44, 44, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 45, 46, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 47, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 50, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 48, 49, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 51, 52, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 53, 54, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 55, 56, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 57, 58, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 62, 61, 91, 59, 91, 91, 60, 64, 91, 63, 91, 91, 64, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 91, 92, 65, 91, 92, 92, 91, 65, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 65, 65, 91, 69, 68, 91, 66, 91, 91, 67, 69, 91, 69, 91, 91, 69, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 91, 92, 70, 91, 92, 92, 91, 70, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 70, 70, 91, 72, 72, 91, 71, 91, 91, 72, 72, 91, 72, 91, 91, 72, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 91, 92, 74, 91, 92, 92, 91, 73, 91, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 75, 76, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 77, 78, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 80, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 79, 79, 91, 91, 91, 91, 91, 91, 91, 91, 91, 82, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 81, 81, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 83, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 84, 91, 92, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 85, 86, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 88, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 87, 87, 91, 91, 91, 91, 91, 91, 91, 91, 91, 90, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 89, 89, 91];
    static OPCODES: [&[OpCode]; 91] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2)], &[OpCode::Exit(3), OpCode::NT(3), OpCode::T(14)], &[OpCode::Exit(4), OpCode::NT(4), OpCode::T(15)], &[OpCode::Exit(5), OpCode::NT(5), OpCode::T(16)], &[OpCode::Exit(6), OpCode::NT(7), OpCode::T(17)], &[OpCode::Exit(7), OpCode::NT(9), OpCode::T(19)], &[OpCode::Exit(8), OpCode::NT(10), OpCode::T(18)], &[OpCode::Exit(9), OpCode::NT(11), OpCode::T(20)], &[OpCode::Exit(10), OpCode::NT(12), OpCode::T(21)], &[OpCode::Exit(11), OpCode::NT(13), OpCode::T(22)], &[OpCode::Exit(12), OpCode::NT(14), OpCode::T(23)], &[OpCode::Exit(13), OpCode::NT(15), OpCode::T(24)], &[OpCode::Exit(14), OpCode::NT(17), OpCode::T(25)], &[OpCode::Exit(15), OpCode::NT(19), OpCode::T(26)], &[OpCode::Exit(16), OpCode::NT(20), OpCode::T(27)], &[OpCode::Exit(17), OpCode::T(12), OpCode::NT(25), OpCode::T(28), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(18), OpCode::T(12), OpCode::NT(26), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(19), OpCode::T(12), OpCode::NT(6), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::Loop(6), OpCode::Exit(20), OpCode::T(29), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(21)], &[OpCode::Exit(22), OpCode::T(12), OpCode::NT(8), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(38), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(24), OpCode::NT(21), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(25), OpCode::NT(22), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(26), OpCode::T(12), OpCode::NT(23), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(27), OpCode::T(12), OpCode::NT(24), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(28), OpCode::T(12), OpCode::T(8), OpCode::NT(27), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(29), OpCode::T(12), OpCode::T(8), OpCode::NT(28), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(30), OpCode::T(12), OpCode::T(8), OpCode::NT(16), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Loop(16), OpCode::Exit(31), OpCode::T(28)], &[OpCode::Loop(16), OpCode::Exit(32), OpCode::T(28), OpCode::T(10), OpCode::T(29)], &[OpCode::Exit(33)], &[OpCode::Exit(34), OpCode::T(12), OpCode::T(8), OpCode::NT(18), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(39), OpCode::T(28)], &[OpCode::NT(40), OpCode::T(28), OpCode::T(10), OpCode::T(29)], &[OpCode::Exit(37), OpCode::T(12), OpCode::NT(29), OpCode::T(29), OpCode::T(10), OpCode::T(28), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(41), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(39), OpCode::NT(21), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(40), OpCode::T(12)], &[OpCode::Loop(22), OpCode::Exit(41), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(42), OpCode::T(12)], &[OpCode::NT(31), OpCode::Exit(43), OpCode::T(29)], &[OpCode::NT(32), OpCode::Exit(44), OpCode::NT(37)], &[OpCode::Loop(25), OpCode::Exit(45), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(46)], &[OpCode::NT(42), OpCode::T(29), OpCode::T(11)], &[OpCode::Loop(27), OpCode::Exit(48), OpCode::T(28)], &[OpCode::Loop(27), OpCode::Exit(49), OpCode::T(28), OpCode::T(10), OpCode::T(29)], &[OpCode::Exit(50)], &[OpCode::NT(43), OpCode::T(28)], &[OpCode::NT(44), OpCode::T(28), OpCode::T(10), OpCode::T(29)], &[OpCode::Loop(29), OpCode::Exit(53), OpCode::T(29), OpCode::T(10), OpCode::T(28), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(54)], &[OpCode::Loop(30), OpCode::Exit(55), OpCode::T(29), OpCode::T(10), OpCode::T(28), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(56)], &[OpCode::Loop(31), OpCode::Exit(57), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(58)], &[OpCode::Loop(32), OpCode::Exit(59), OpCode::NT(35), OpCode::T(3)], &[OpCode::Loop(32), OpCode::Exit(60), OpCode::NT(35), OpCode::T(6)], &[OpCode::Loop(32), OpCode::Exit(61), OpCode::NT(35), OpCode::T(1)], &[OpCode::Loop(32), OpCode::Exit(62), OpCode::NT(33), OpCode::T(0)], &[OpCode::Loop(32), OpCode::Exit(63), OpCode::NT(33), OpCode::T(9)], &[OpCode::Exit(64)], &[OpCode::NT(34), OpCode::Exit(65), OpCode::NT(37)], &[OpCode::Loop(34), OpCode::Exit(66), OpCode::NT(35), OpCode::T(3)], &[OpCode::Loop(34), OpCode::Exit(67), OpCode::NT(35), OpCode::T(6)], &[OpCode::Loop(34), OpCode::Exit(68), OpCode::NT(35), OpCode::T(1)], &[OpCode::Exit(69)], &[OpCode::NT(36), OpCode::Exit(70), OpCode::NT(37)], &[OpCode::Loop(36), OpCode::Exit(71), OpCode::NT(35), OpCode::T(3)], &[OpCode::Exit(72)], &[OpCode::Exit(73), OpCode::NT(24), OpCode::T(9)], &[OpCode::Exit(74), OpCode::T(7), OpCode::NT(24), OpCode::T(4)], &[OpCode::Exit(75), OpCode::T(28)], &[OpCode::Exit(76), OpCode::T(29)], &[OpCode::Loop(8), OpCode::Exit(77)], &[OpCode::Exit(78)], &[OpCode::Loop(18), OpCode::Exit(79)], &[OpCode::Exit(80)], &[OpCode::Loop(18), OpCode::Exit(81)], &[OpCode::Exit(82)], &[OpCode::Exit(83), OpCode::T(12)], &[OpCode::Exit(84), OpCode::T(12), OpCode::NT(30), OpCode::T(29), OpCode::T(10), OpCode::T(28)], &[OpCode::Loop(26), OpCode::Exit(85)], &[OpCode::Exit(86)], &[OpCode::Loop(28), OpCode::Exit(87)], &[OpCode::Exit(88)], &[OpCode::Loop(28), OpCode::Exit(89)], &[OpCode::Exit(90)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        Parser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            Vec::new(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    #[derive(Debug)]
    pub enum CtxText {
        /// `text -> (<L> example)*`
        V1 { star: SynI },
    }
    #[derive(Debug)]
    pub enum CtxI {
        /// `<L> example` iteration in `text -> ( ►► <L> example ◄◄ )*`
        V1 { example: SynExample },
    }
    #[derive(Debug)]
    pub enum CtxExample {
        /// `example -> "star" star`
        V1 { star: SynStar },
        /// `example -> "plus" plus`
        V2 { plus: SynPlus },
        /// `example -> "l-star" l_star`
        V3 { l_star: SynLStar },
        /// `example -> "l-plus" l_plus`
        V4 { l_plus: SynLPlus },
        /// `example -> "rrec" rrec`
        V5 { rrec: SynRrec },
        /// `example -> "l-rrec" l_rrec`
        V6 { l_rrec: SynLRrec },
        /// `example -> "lrec" lrec`
        V7 { lrec: SynLrec },
        /// `example -> "amb" amb`
        V8 { amb: SynAmb },
        /// `example -> "star-a" star_a`
        V9 { star_a: SynStarA },
        /// `example -> "plus-a" plus_a`
        V10 { plus_a: SynPlusA },
        /// `example -> "l-star-a" l_star_a`
        V11 { l_star_a: SynLStarA },
        /// `example -> "l-plus-a" l_plus_a`
        V12 { l_plus_a: SynLPlusA },
        /// `example -> "sep-list" sep_list`
        V13 { sep_list: SynSepList },
        /// `example -> "sep-list-opt" sep_list_opt`
        V14 { sep_list_opt: SynSepListOpt },
    }
    #[derive(Debug)]
    pub enum CtxStar {
        /// `star -> Id "=" Id ("," Num)* ";"`
        V1 { id: [String; 2], star: SynStar1 },
    }
    #[derive(Debug)]
    pub enum CtxPlus {
        /// `plus -> Id "=" Num ("," Num)+ ";"`
        V1 { id: String, num: String, plus: SynPlus1 },
    }
    #[derive(Debug)]
    pub enum CtxLStar {
        /// `l_star -> Id "=" Num (<L> "," "then" Num)* ";"`
        V1 { id: String, star: SynLStarI },
    }
    #[derive(Debug)]
    pub enum InitCtxLStarI {
        /// value of `Num` before `<L> "," "then" Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," "then" Num ◄◄ )* ";"`
        V1 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxLStarI {
        /// `<L> "," "then" Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," "then" Num ◄◄ )* ";"`
        V1 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxLPlus {
        /// `l_plus -> Id "=" Num (<L> "," Num)+ ";"`
        V1 { id: String, num: String, plus: SynLPlusI },
    }
    #[derive(Debug)]
    pub enum CtxLPlusI {
        /// `<L> "," Num` iteration in `l_plus -> Id "=" Num ( ►► <L> "," Num ◄◄ )+ ";"`
        V1 { num: String, last_iteration: bool },
    }
    #[derive(Debug)]
    pub enum CtxRrec {
        /// `rrec -> Id "=" Num rrec_i`
        V1 { id: String, num: String, rrec_i: SynRrecI },
    }
    #[derive(Debug)]
    pub enum CtxLRrec {
        /// `l_rrec -> Id "=" Num l_rrec_i`
        V1 { id: String, num: String, l_rrec_i: SynLRrecI },
    }
    #[derive(Debug)]
    pub enum CtxLrec {
        /// `lrec -> Id "=" lrec_i ";"`
        V1 { id: String, lrec_i: SynLrecI },
    }
    #[derive(Debug)]
    pub enum CtxAmb {
        /// `amb -> Id "=" amb_i ";"`
        V1 { id: String, amb_i: SynAmbI },
    }
    #[derive(Debug)]
    pub enum CtxStarA {
        /// `star_a -> Id "=" "[" (Id | Num ":" Id)* "]" ";"`
        V1 { id: String, star: SynStarA1 },
    }
    #[derive(Debug)]
    pub enum CtxPlusA {
        /// `plus_a -> Id "=" "[" (Id | Num ":" Id)+ "]" ";"`
        V1 { id: String, plus: SynPlusA1 },
    }
    #[derive(Debug)]
    pub enum CtxLStarA {
        /// `l_star_a -> Id "=" "[" (<L> Id | Num ":" Id)* "]" ";"`
        V1 { id: String, star: SynLStarAI },
    }
    #[derive(Debug)]
    pub enum CtxLStarAI {
        /// `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
        V1 { id: String },
        /// `Num ":" Id` iteration in `l_star_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )* "]" ";"`
        V2 { num: String, id: String },
    }
    #[derive(Debug)]
    pub enum CtxLPlusA {
        /// `l_plus_a -> Id "=" "[" (<L> Id | Num ":" Id)+ "]" ";"`
        V1 { id: String, plus: SynLPlusAI },
    }
    #[derive(Debug)]
    pub enum CtxLPlusAI {
        /// `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
        V1 { id: String, last_iteration: bool },
        /// `Num ":" Id` iteration in `l_plus_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
        V2 { num: String, id: String, last_iteration: bool },
    }
    #[derive(Debug)]
    pub enum CtxSepList {
        /// `sep_list -> Id "=" Id ":" Num ("," "then" Id ":" Num)* ";"`
        V1 { id: String, star: SynSepList1 },
    }
    #[derive(Debug)]
    pub enum CtxSepListOpt {
        /// `sep_list_opt -> Id "=" Id ":" Num ("," "then" Id ":" Num)* ";"`
        V1 { id: String, star: SynSepListOpt1 },
        /// `sep_list_opt -> Id "=" ";"`
        V2 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxRrecI {
        /// `rrec_i -> "," Num rrec_i`
        V1 { num: String, rrec_i: SynRrecI },
        /// `rrec_i -> ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxLRrecI {
        /// `l_rrec_i -> <L> "," Num l_rrec_i`
        V1 { num: String },
        /// `l_rrec_i -> ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxLrecI {
        /// `lrec_i -> lrec_i "," Num`
        V1 { lrec_i: SynLrecI, num: String },
        /// `lrec_i -> Num`
        V2 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxAmbI {
        /// `amb_i -> <R> amb_i "^" amb_i`
        V1 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i "*" amb_i`
        V2 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i <P> "/" amb_i`
        V3 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i "+" amb_i`
        V4 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i <P> "-" amb_i`
        V5 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> "-" amb_i`
        V6 { amb_i: SynAmbI },
        /// `amb_i -> "(" amb_i ")"`
        V7 { amb_i: SynAmbI },
        /// `amb_i -> Id`
        V8 { id: String },
        /// `amb_i -> Num`
        V9 { num: String },
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `text`
    // #[derive(Debug, PartialEq)] pub struct SynText();
    // /// User-defined type for `<L> example` iteration in `text -> ( ►► <L> example ◄◄ )*`
    // #[derive(Debug, PartialEq)] pub struct SynI();
    // /// User-defined type for `example`
    // #[derive(Debug, PartialEq)] pub struct SynExample();
    // /// User-defined type for `star`
    // #[derive(Debug, PartialEq)] pub struct SynStar();
    // /// User-defined type for `plus`
    // #[derive(Debug, PartialEq)] pub struct SynPlus();
    // /// User-defined type for `l_star`
    // #[derive(Debug, PartialEq)] pub struct SynLStar();
    // /// User-defined type for `<L> "," "then" Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," "then" Num ◄◄ )* ";"`
    // #[derive(Debug, PartialEq)] pub struct SynLStarI();
    // /// User-defined type for `l_plus`
    // #[derive(Debug, PartialEq)] pub struct SynLPlus();
    // /// User-defined type for `<L> "," Num` iteration in `l_plus -> Id "=" Num ( ►► <L> "," Num ◄◄ )+ ";"`
    // #[derive(Debug, PartialEq)] pub struct SynLPlusI();
    // /// User-defined type for `rrec`
    // #[derive(Debug, PartialEq)] pub struct SynRrec();
    // /// User-defined type for `l_rrec`
    // #[derive(Debug, PartialEq)] pub struct SynLRrec();
    // /// User-defined type for `lrec`
    // #[derive(Debug, PartialEq)] pub struct SynLrec();
    // /// User-defined type for `amb`
    // #[derive(Debug, PartialEq)] pub struct SynAmb();
    // /// User-defined type for `star_a`
    // #[derive(Debug, PartialEq)] pub struct SynStarA();
    // /// User-defined type for `plus_a`
    // #[derive(Debug, PartialEq)] pub struct SynPlusA();
    // /// User-defined type for `l_star_a`
    // #[derive(Debug, PartialEq)] pub struct SynLStarA();
    // /// User-defined type for `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
    // #[derive(Debug, PartialEq)] pub struct SynLStarAI();
    // /// User-defined type for `l_plus_a`
    // #[derive(Debug, PartialEq)] pub struct SynLPlusA();
    // /// User-defined type for `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
    // #[derive(Debug, PartialEq)] pub struct SynLPlusAI();
    // /// User-defined type for `sep_list`
    // #[derive(Debug, PartialEq)] pub struct SynSepList();
    // /// User-defined type for `sep_list_opt`
    // #[derive(Debug, PartialEq)] pub struct SynSepListOpt();
    // /// User-defined type for `rrec_i`
    // #[derive(Debug, PartialEq)] pub struct SynRrecI();
    // /// User-defined type for `l_rrec_i`
    // #[derive(Debug, PartialEq)] pub struct SynLRrecI();
    // /// User-defined type for `lrec_i`
    // #[derive(Debug, PartialEq)] pub struct SynLrecI();
    // /// User-defined type for `amb_i`
    // #[derive(Debug, PartialEq)] pub struct SynAmbI();
    /// Computed `("," Num)*` array in `star -> Id "=" Id  ►► ("," Num)* ◄◄  ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynStar1(pub Vec<String>);
    /// Computed `("," Num)+` array in `plus -> Id "=" Num  ►► ("," Num)+ ◄◄  ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynPlus1(pub Vec<String>);
    /// Computed `(Id | Num ":" Id)*` array in `star_a -> Id "=" "["  ►► (Id | Num ":" Id)* ◄◄  "]" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynStarA1(pub Vec<SynStarA1Item>);
    #[derive(Debug, PartialEq)]
    pub enum SynStarA1Item {
        /// `Id` item in `star_a -> Id "=" "[" ( ►► Id ◄◄  | Num ":" Id)* "]" ";"`
        V1 { id: String },
        /// `Num ":" Id` item in `star_a -> Id "=" "[" (Id |  ►► Num ":" Id ◄◄ )* "]" ";"`
        V2 { num: String, id: String },
    }
    /// Computed `(Id | Num ":" Id)+` array in `plus_a -> Id "=" "["  ►► (Id | Num ":" Id)+ ◄◄  "]" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynPlusA1(pub Vec<SynPlusA1Item>);
    #[derive(Debug, PartialEq)]
    pub enum SynPlusA1Item {
        /// `Id` item in `plus_a -> Id "=" "[" ( ►► Id ◄◄  | Num ":" Id)+ "]" ";"`
        V1 { id: String },
        /// `Num ":" Id` item in `plus_a -> Id "=" "[" (Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
        V2 { num: String, id: String },
    }
    /// Computed `("," "then" Id ":" Num)*` array in `sep_list -> Id "=" Id ":" Num  ►► ("," "then" Id ":" Num)* ◄◄  ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynSepList1(pub Vec<SynSepList1Item>);
    /// `"," "then" Id ":" Num` item in `sep_list -> Id "=" Id ":" Num ( ►► "," "then" Id ":" Num ◄◄ )* ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynSepList1Item { pub id: String, pub num: String }
    /// Computed `("," "then" Id ":" Num)*` array in `sep_list_opt -> Id "=" Id ":" Num  ►► ("," "then" Id ":" Num)* ◄◄  ";" | Id "=" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynSepListOpt1(pub Vec<SynSepListOpt1Item>);
    /// `"," "then" Id ":" Num` item in `sep_list_opt -> Id "=" Id ":" Num ( ►► "," "then" Id ":" Num ◄◄ )* ";" | Id "=" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynSepListOpt1Item { pub id: String, pub num: String }

    #[derive(Debug)]
    enum SynValue { Text(SynText), I(SynI), Example(SynExample), Star(SynStar), Plus(SynPlus), LStar(SynLStar), LStarI(SynLStarI), LPlus(SynLPlus), LPlusI(SynLPlusI), Rrec(SynRrec), LRrec(SynLRrec), Lrec(SynLrec), Amb(SynAmb), StarA(SynStarA), PlusA(SynPlusA), LStarA(SynLStarA), LStarAI(SynLStarAI), LPlusA(SynLPlusA), LPlusAI(SynLPlusAI), SepList(SynSepList), SepListOpt(SynSepListOpt), RrecI(SynRrecI), LRrecI(SynLRrecI), LrecI(SynLrecI), AmbI(SynAmbI), Star1(SynStar1), Plus1(SynPlus1), StarA1(SynStarA1), PlusA1(SynPlusA1), SepList1(SynSepList1), SepListOpt1(SynSepListOpt1) }

    impl SynValue {
        fn get_text(self) -> SynText {
            if let SynValue::Text(val) = self { val } else { panic!() }
        }
        fn get_i(self) -> SynI {
            if let SynValue::I(val) = self { val } else { panic!() }
        }
        fn get_example(self) -> SynExample {
            if let SynValue::Example(val) = self { val } else { panic!() }
        }
        fn get_star(self) -> SynStar {
            if let SynValue::Star(val) = self { val } else { panic!() }
        }
        fn get_plus(self) -> SynPlus {
            if let SynValue::Plus(val) = self { val } else { panic!() }
        }
        fn get_l_star(self) -> SynLStar {
            if let SynValue::LStar(val) = self { val } else { panic!() }
        }
        fn get_l_star_i(self) -> SynLStarI {
            if let SynValue::LStarI(val) = self { val } else { panic!() }
        }
        fn get_l_plus(self) -> SynLPlus {
            if let SynValue::LPlus(val) = self { val } else { panic!() }
        }
        fn get_l_plus_i(self) -> SynLPlusI {
            if let SynValue::LPlusI(val) = self { val } else { panic!() }
        }
        fn get_rrec(self) -> SynRrec {
            if let SynValue::Rrec(val) = self { val } else { panic!() }
        }
        fn get_l_rrec(self) -> SynLRrec {
            if let SynValue::LRrec(val) = self { val } else { panic!() }
        }
        fn get_lrec(self) -> SynLrec {
            if let SynValue::Lrec(val) = self { val } else { panic!() }
        }
        fn get_amb(self) -> SynAmb {
            if let SynValue::Amb(val) = self { val } else { panic!() }
        }
        fn get_star_a(self) -> SynStarA {
            if let SynValue::StarA(val) = self { val } else { panic!() }
        }
        fn get_plus_a(self) -> SynPlusA {
            if let SynValue::PlusA(val) = self { val } else { panic!() }
        }
        fn get_l_star_a(self) -> SynLStarA {
            if let SynValue::LStarA(val) = self { val } else { panic!() }
        }
        fn get_l_star_a_i(self) -> SynLStarAI {
            if let SynValue::LStarAI(val) = self { val } else { panic!() }
        }
        fn get_l_plus_a(self) -> SynLPlusA {
            if let SynValue::LPlusA(val) = self { val } else { panic!() }
        }
        fn get_l_plus_a_i(self) -> SynLPlusAI {
            if let SynValue::LPlusAI(val) = self { val } else { panic!() }
        }
        fn get_sep_list(self) -> SynSepList {
            if let SynValue::SepList(val) = self { val } else { panic!() }
        }
        fn get_sep_list_opt(self) -> SynSepListOpt {
            if let SynValue::SepListOpt(val) = self { val } else { panic!() }
        }
        fn get_rrec_i(self) -> SynRrecI {
            if let SynValue::RrecI(val) = self { val } else { panic!() }
        }
        fn get_l_rrec_i(self) -> SynLRrecI {
            if let SynValue::LRrecI(val) = self { val } else { panic!() }
        }
        fn get_lrec_i(self) -> SynLrecI {
            if let SynValue::LrecI(val) = self { val } else { panic!() }
        }
        fn get_amb_i(self) -> SynAmbI {
            if let SynValue::AmbI(val) = self { val } else { panic!() }
        }
        fn get_star1(self) -> SynStar1 {
            if let SynValue::Star1(val) = self { val } else { panic!() }
        }
        fn get_plus1(self) -> SynPlus1 {
            if let SynValue::Plus1(val) = self { val } else { panic!() }
        }
        fn get_star_a1(self) -> SynStarA1 {
            if let SynValue::StarA1(val) = self { val } else { panic!() }
        }
        fn get_plus_a1(self) -> SynPlusA1 {
            if let SynValue::PlusA1(val) = self { val } else { panic!() }
        }
        fn get_sep_list1(self) -> SynSepList1 {
            if let SynValue::SepList1(val) = self { val } else { panic!() }
        }
        fn get_sep_list_opt1(self) -> SynSepListOpt1 {
            if let SynValue::SepListOpt1(val) = self { val } else { panic!() }
        }
    }

    pub trait PandemoniumListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, text: SynText, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_text(&mut self) {}
        fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) -> SynText;
        fn init_i(&mut self) -> SynI;
        fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_i(&mut self, acc: &mut SynI) {}
        fn init_example(&mut self) {}
        fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) -> SynExample;
        fn init_star(&mut self) {}
        fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar;
        fn init_plus(&mut self) {}
        fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) -> SynPlus;
        fn init_l_star(&mut self) {}
        fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) -> SynLStar;
        fn init_l_star_i(&mut self, ctx: InitCtxLStarI, spans: Vec<PosSpan>) -> SynLStarI;
        fn exit_l_star_i(&mut self, acc: &mut SynLStarI, ctx: CtxLStarI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_l_star_i(&mut self, acc: &mut SynLStarI) {}
        fn init_l_plus(&mut self) {}
        fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) -> SynLPlus;
        fn init_l_plus_i(&mut self) -> SynLPlusI;
        fn exit_l_plus_i(&mut self, acc: &mut SynLPlusI, ctx: CtxLPlusI, spans: Vec<PosSpan>);
        fn init_rrec(&mut self) {}
        fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) -> SynRrec;
        fn init_l_rrec(&mut self) {}
        fn exit_l_rrec(&mut self, ctx: CtxLRrec, spans: Vec<PosSpan>) -> SynLRrec;
        fn init_lrec(&mut self) {}
        fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) -> SynLrec;
        fn init_amb(&mut self) {}
        fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) -> SynAmb;
        fn init_star_a(&mut self) {}
        fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) -> SynStarA;
        fn init_plus_a(&mut self) {}
        fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) -> SynPlusA;
        fn init_l_star_a(&mut self) {}
        fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) -> SynLStarA;
        fn init_l_star_a_i(&mut self) -> SynLStarAI;
        fn exit_l_star_a_i(&mut self, acc: &mut SynLStarAI, ctx: CtxLStarAI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_l_star_a_i(&mut self, acc: &mut SynLStarAI) {}
        fn init_l_plus_a(&mut self) {}
        fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) -> SynLPlusA;
        fn init_l_plus_a_i(&mut self) -> SynLPlusAI;
        fn exit_l_plus_a_i(&mut self, acc: &mut SynLPlusAI, ctx: CtxLPlusAI, spans: Vec<PosSpan>);
        fn init_sep_list(&mut self) {}
        fn exit_sep_list(&mut self, ctx: CtxSepList, spans: Vec<PosSpan>) -> SynSepList;
        fn init_sep_list_opt(&mut self) {}
        fn exit_sep_list_opt(&mut self, ctx: CtxSepListOpt, spans: Vec<PosSpan>) -> SynSepListOpt;
        fn init_rrec_i(&mut self) {}
        fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) -> SynRrecI;
        fn init_l_rrec_i(&mut self) -> SynLRrecI;
        fn exit_l_rrec_i(&mut self, acc: &mut SynLRrecI, ctx: CtxLRrecI, spans: Vec<PosSpan>);
        fn init_lrec_i(&mut self) {}
        fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) -> SynLrecI;
        #[allow(unused_variables)]
        fn exitloop_lrec_i(&mut self, lrec_i: &mut SynLrecI) {}
        fn init_amb_i(&mut self) {}
        fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) -> SynAmbI;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
        stack_span: Vec<PosSpan>,
    }

    impl<T: PandemoniumListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    if matches!(nt, 1 | 8 | 16 | 18 | 22 | 25 ..= 28) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_text(),             // text
                        1 => self.init_i(),                         // i
                        2 => self.listener.init_example(),          // example
                        3 => self.listener.init_star(),             // star
                        25 => self.init_star1(),                    // star_1
                        4 => self.listener.init_plus(),             // plus
                        26 => self.init_plus1(),                    // plus_1
                        42 => {}                                    // plus_2
                        5 => self.listener.init_l_star(),           // l_star
                        6 => self.init_l_star_i(),                  // l_star_i
                        7 => self.listener.init_l_plus(),           // l_plus
                        8 => self.init_l_plus_i(),                  // l_plus_i
                        38 => {}                                    // l_plus_i_1
                        9 => self.listener.init_rrec(),             // rrec
                        10 => self.listener.init_l_rrec(),          // l_rrec
                        11 => self.listener.init_lrec(),            // lrec
                        12 => self.listener.init_amb(),             // amb
                        13 => self.listener.init_star_a(),          // star_a
                        27 => self.init_star_a1(),                  // star_a_1
                        14 => self.listener.init_plus_a(),          // plus_a
                        28 => self.init_plus_a1(),                  // plus_a_1
                        43 | 44 => {}                               // plus_a_2, plus_a_3
                        15 => self.listener.init_l_star_a(),        // l_star_a
                        16 => self.init_l_star_a_i(),               // l_star_a_i
                        17 => self.listener.init_l_plus_a(),        // l_plus_a
                        18 => self.init_l_plus_a_i(),               // l_plus_a_i
                        39 | 40 => {}                               // l_plus_a_i_1, l_plus_a_i_2
                        19 => self.listener.init_sep_list(),        // sep_list
                        29 => self.init_sep_list1(),                // sep_list_1
                        20 => self.listener.init_sep_list_opt(),    // sep_list_opt
                        30 => self.init_sep_list_opt1(),            // sep_list_opt_1
                        41 => {}                                    // sep_list_opt_2
                        21 => self.listener.init_rrec_i(),          // rrec_i
                        22 => self.init_l_rrec_i(),                 // l_rrec_i
                        23 => self.listener.init_lrec_i(),          // lrec_i
                        31 => {}                                    // lrec_i_1
                        24 => self.listener.init_amb_i(),           // amb_i
                        32 ..= 37 => {}                             // amb_i_1, amb_i_2, amb_i_3, amb_i_4, amb_i_5, amb_i_6
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_text(),                      // text -> i
                        1 => self.exit_i(),                         // i -> <L> example i
                        2 => self.exitloop_i(),                     // i -> <L> ε
                        3 |                                         // example -> "star" star
                        4 |                                         // example -> "plus" plus
                        5 |                                         // example -> "l-star" l_star
                        6 |                                         // example -> "l-plus" l_plus
                        7 |                                         // example -> "rrec" rrec
                        8 |                                         // example -> "l-rrec" l_rrec
                        9 |                                         // example -> "lrec" lrec
                        10 |                                        // example -> "amb" amb
                        11 |                                        // example -> "star-a" star_a
                        12 |                                        // example -> "plus-a" plus_a
                        13 |                                        // example -> "l-star-a" l_star_a
                        14 |                                        // example -> "l-plus-a" l_plus_a
                        15 |                                        // example -> "sep-list" sep_list
                        16 => self.exit_example(alt_id),            // example -> "sep-list-opt" sep_list_opt
                        17 => self.exit_star(),                     // star -> Id "=" Id star_1 ";"
                        45 => self.exit_star1(),                    // star_1 -> "," Num star_1
                        46 => {}                                    // star_1 -> ε
                        18 => self.exit_plus(),                     // plus -> Id "=" Num plus_1 ";"
                        85 |                                        // plus_2 -> plus_1
                        86 => self.exit_plus1(),                    // plus_2 -> ε
                     /* 47 */                                       // plus_1 -> "," Num plus_2 (never called)
                        19 => self.exit_l_star(),                   // l_star -> Id "=" Num l_star_i ";"
                        20 => self.exit_l_star_i(),                 // l_star_i -> <L> "," "then" Num l_star_i
                        21 => self.exitloop_l_star_i(),             // l_star_i -> <L> ε
                        22 => self.exit_l_plus(),                   // l_plus -> Id "=" Num l_plus_i ";"
                        77 |                                        // l_plus_i_1 -> l_plus_i
                        78 => self.exit_l_plus_i(alt_id),           // l_plus_i_1 -> ε
                     /* 23 */                                       // l_plus_i -> <L> "," Num l_plus_i_1 (never called)
                        24 => self.exit_rrec(),                     // rrec -> Id "=" Num rrec_i
                        25 => self.exit_l_rrec(),                   // l_rrec -> Id "=" Num l_rrec_i
                        26 => self.exit_lrec(),                     // lrec -> Id "=" lrec_i ";"
                        27 => self.exit_amb(),                      // amb -> Id "=" amb_i ";"
                        28 => self.exit_star_a(),                   // star_a -> Id "=" "[" star_a_1 "]" ";"
                        48 |                                        // star_a_1 -> Id star_a_1
                        49 => self.exit_star_a1(alt_id),            // star_a_1 -> Num ":" Id star_a_1
                        50 => {}                                    // star_a_1 -> ε
                        29 => self.exit_plus_a(),                   // plus_a -> Id "=" "[" plus_a_1 "]" ";"
                        87 |                                        // plus_a_2 -> plus_a_1
                        88 |                                        // plus_a_2 -> ε
                        89 |                                        // plus_a_3 -> plus_a_1
                        90 => self.exit_plus_a1(alt_id),            // plus_a_3 -> ε
                     /* 51 */                                       // plus_a_1 -> Id plus_a_2 (never called)
                     /* 52 */                                       // plus_a_1 -> Num ":" Id plus_a_3 (never called)
                        30 => self.exit_l_star_a(),                 // l_star_a -> Id "=" "[" l_star_a_i "]" ";"
                        31 |                                        // l_star_a_i -> <L> Id l_star_a_i
                        32 => self.exit_l_star_a_i(alt_id),         // l_star_a_i -> <L> Num ":" Id l_star_a_i
                        33 => self.exitloop_l_star_a_i(),           // l_star_a_i -> <L> ε
                        34 => self.exit_l_plus_a(),                 // l_plus_a -> Id "=" "[" l_plus_a_i "]" ";"
                        79 |                                        // l_plus_a_i_1 -> l_plus_a_i
                        80 |                                        // l_plus_a_i_1 -> ε
                        81 |                                        // l_plus_a_i_2 -> l_plus_a_i
                        82 => self.exit_l_plus_a_i(alt_id),         // l_plus_a_i_2 -> ε
                     /* 35 */                                       // l_plus_a_i -> <L> Id l_plus_a_i_1 (never called)
                     /* 36 */                                       // l_plus_a_i -> <L> Num ":" Id l_plus_a_i_2 (never called)
                        37 => self.exit_sep_list(),                 // sep_list -> Id "=" Id ":" Num sep_list_1 ";"
                        53 => self.exit_sep_list1(),                // sep_list_1 -> "," "then" Id ":" Num sep_list_1
                        54 => {}                                    // sep_list_1 -> ε
                        83 |                                        // sep_list_opt_2 -> ";"
                        84 => self.exit_sep_list_opt(alt_id),       // sep_list_opt_2 -> Id ":" Num sep_list_opt_1 ";"
                        55 => self.exit_sep_list_opt1(),            // sep_list_opt_1 -> "," "then" Id ":" Num sep_list_opt_1
                        56 => {}                                    // sep_list_opt_1 -> ε
                     /* 38 */                                       // sep_list_opt -> Id "=" sep_list_opt_2 (never called)
                        39 |                                        // rrec_i -> "," Num rrec_i
                        40 => self.exit_rrec_i(alt_id),             // rrec_i -> ";"
                        41 |                                        // l_rrec_i -> <L> "," Num l_rrec_i
                        42 => self.exit_l_rrec_i(alt_id),           // l_rrec_i -> <L> ";"
                        43 => self.inter_lrec_i(),                  // lrec_i -> Num lrec_i_1
                        57 => self.exit_lrec_i1(),                  // lrec_i_1 -> "," Num lrec_i_1
                        58 => self.exitloop_lrec_i1(),              // lrec_i_1 -> ε
                        59 |                                        // amb_i_1 -> <R> "^" amb_i_4 amb_i_1
                        60 |                                        // amb_i_1 -> "*" amb_i_4 amb_i_1
                        61 |                                        // amb_i_1 -> "/" amb_i_4 amb_i_1
                        62 |                                        // amb_i_1 -> "+" amb_i_2 amb_i_1
                        63 => self.exit_amb_i1(alt_id),             // amb_i_1 -> "-" amb_i_2 amb_i_1
                        66 |                                        // amb_i_3 -> <R> "^" amb_i_4 amb_i_3 (duplicate of 59)
                        71 => self.exit_amb_i1(59),                 // amb_i_5 -> <R> "^" amb_i_4 amb_i_5 (duplicate of 59)
                        67 => self.exit_amb_i1(60),                 // amb_i_3 -> "*" amb_i_4 amb_i_3 (duplicate of 60)
                        68 => self.exit_amb_i1(61),                 // amb_i_3 -> "/" amb_i_4 amb_i_3 (duplicate of 61)
                        73 |                                        // amb_i_6 -> "-" amb_i
                        74 |                                        // amb_i_6 -> "(" amb_i ")"
                        75 |                                        // amb_i_6 -> Id
                        76 => self.exit_amb_i6(alt_id),             // amb_i_6 -> Num
                        44 => {}                                    // amb_i -> amb_i_6 amb_i_1 (not used)
                        64 => {}                                    // amb_i_1 -> ε (not used)
                        65 => {}                                    // amb_i_2 -> amb_i_6 amb_i_3 (not used)
                        69 => {}                                    // amb_i_3 -> ε (not used)
                        70 => {}                                    // amb_i_4 -> amb_i_6 amb_i_5 (not used)
                        72 => {}                                    // amb_i_5 -> ε (not used)
                        _ => panic!("unexpected exit alternative id: {alt_id}")
                    }
                }
                Call::End(terminate) => {
                    match terminate {
                        Terminate::None => {
                            let val = self.stack.pop().unwrap().get_text();
                            let span = self.stack_span.pop().unwrap();
                            self.listener.exit(val, span);
                        }
                        Terminate::Abort | Terminate::Conclude => self.listener.abort(terminate),
                    }
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).collect::<Vec<_>>().join(", "));
            }
        }

        fn check_abort_request(&self) -> Terminate {
            self.listener.check_abort_request()
        }

        fn abort(&mut self) {
            self.stack.clear();
            self.stack_span.clear();
            self.stack_t.clear();
        }

        fn get_mut_log(&mut self) -> &mut impl Logger {
            self.listener.get_mut_log()
        }

        fn push_span(&mut self, span: PosSpan) {
            self.stack_span.push(span);
        }

        fn is_stack_empty(&self) -> bool {
            self.stack.is_empty()
        }

        fn is_stack_t_empty(&self) -> bool {
            self.stack_t.is_empty()
        }

        fn is_stack_span_empty(&self) -> bool {
            self.stack_span.is_empty()
        }

        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
            self.listener.intercept_token(token, text, span)
        }
    }

    impl<T: PandemoniumListener> Wrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new(), stack_span: Vec::new() }
        }

        pub fn get_listener(&self) -> &T {
            &self.listener
        }

        pub fn get_listener_mut(&mut self) -> &mut T {
            &mut self.listener
        }

        pub fn give_listener(self) -> T {
            self.listener
        }

        pub fn set_verbose(&mut self, verbose: bool) {
            self.verbose = verbose;
        }

        fn exit_text(&mut self) {
            let star = self.stack.pop().unwrap().get_i();
            let ctx = CtxText::V1 { star };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_text(ctx, spans);
            self.stack.push(SynValue::Text(val));
        }

        fn init_i(&mut self) {
            let val = self.listener.init_i();
            self.stack.push(SynValue::I(val));
        }

        fn exit_i(&mut self) {
            let example = self.stack.pop().unwrap().get_example();
            let ctx = CtxI::V1 { example };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::I(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_i(acc, ctx, spans);
        }

        fn exitloop_i(&mut self) {
            let SynValue::I(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_i(acc);
        }

        fn exit_example(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                3 => {
                    let star = self.stack.pop().unwrap().get_star();
                    (2, CtxExample::V1 { star })
                }
                4 => {
                    let plus = self.stack.pop().unwrap().get_plus();
                    (2, CtxExample::V2 { plus })
                }
                5 => {
                    let l_star = self.stack.pop().unwrap().get_l_star();
                    (2, CtxExample::V3 { l_star })
                }
                6 => {
                    let l_plus = self.stack.pop().unwrap().get_l_plus();
                    (2, CtxExample::V4 { l_plus })
                }
                7 => {
                    let rrec = self.stack.pop().unwrap().get_rrec();
                    (2, CtxExample::V5 { rrec })
                }
                8 => {
                    let l_rrec = self.stack.pop().unwrap().get_l_rrec();
                    (2, CtxExample::V6 { l_rrec })
                }
                9 => {
                    let lrec = self.stack.pop().unwrap().get_lrec();
                    (2, CtxExample::V7 { lrec })
                }
                10 => {
                    let amb = self.stack.pop().unwrap().get_amb();
                    (2, CtxExample::V8 { amb })
                }
                11 => {
                    let star_a = self.stack.pop().unwrap().get_star_a();
                    (2, CtxExample::V9 { star_a })
                }
                12 => {
                    let plus_a = self.stack.pop().unwrap().get_plus_a();
                    (2, CtxExample::V10 { plus_a })
                }
                13 => {
                    let l_star_a = self.stack.pop().unwrap().get_l_star_a();
                    (2, CtxExample::V11 { l_star_a })
                }
                14 => {
                    let l_plus_a = self.stack.pop().unwrap().get_l_plus_a();
                    (2, CtxExample::V12 { l_plus_a })
                }
                15 => {
                    let sep_list = self.stack.pop().unwrap().get_sep_list();
                    (2, CtxExample::V13 { sep_list })
                }
                16 => {
                    let sep_list_opt = self.stack.pop().unwrap().get_sep_list_opt();
                    (2, CtxExample::V14 { sep_list_opt })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_example")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_example(ctx, spans);
            self.stack.push(SynValue::Example(val));
        }

        fn exit_star(&mut self) {
            let star = self.stack.pop().unwrap().get_star1();
            let id_2 = self.stack_t.pop().unwrap();
            let id_1 = self.stack_t.pop().unwrap();
            let ctx = CtxStar::V1 { id: [id_1, id_2], star };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_star(ctx, spans);
            self.stack.push(SynValue::Star(val));
        }

        fn init_star1(&mut self) {
            let val = SynStar1(Vec::new());
            self.stack.push(SynValue::Star1(val));
        }

        fn exit_star1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let num = self.stack_t.pop().unwrap();
            let Some(SynValue::Star1(SynStar1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynStar1 item on wrapper stack");
            };
            star_acc.push(num);
        }

        fn exit_plus(&mut self) {
            let plus = self.stack.pop().unwrap().get_plus1();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxPlus::V1 { id, num, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_plus(ctx, spans);
            self.stack.push(SynValue::Plus(val));
        }

        fn init_plus1(&mut self) {
            let val = SynPlus1(Vec::new());
            self.stack.push(SynValue::Plus1(val));
        }

        fn exit_plus1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let num = self.stack_t.pop().unwrap();
            let Some(SynValue::Plus1(SynPlus1(plus_acc))) = self.stack.last_mut() else {
                panic!("expected SynPlus1 item on wrapper stack");
            };
            plus_acc.push(num);
        }

        fn exit_l_star(&mut self) {
            let star = self.stack.pop().unwrap().get_l_star_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLStar::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_star(ctx, spans);
            self.stack.push(SynValue::LStar(val));
        }

        fn init_l_star_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = InitCtxLStarI::V1 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.init_l_star_i(ctx, spans);
            self.stack.push(SynValue::LStarI(val));
        }

        fn exit_l_star_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLStarI::V1 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::LStarI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_star_i(acc, ctx, spans);
        }

        fn exitloop_l_star_i(&mut self) {
            let SynValue::LStarI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_l_star_i(acc);
        }

        fn exit_l_plus(&mut self) {
            let plus = self.stack.pop().unwrap().get_l_plus_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLPlus::V1 { id, num, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_plus(ctx, spans);
            self.stack.push(SynValue::LPlus(val));
        }

        fn init_l_plus_i(&mut self) {
            let val = self.listener.init_l_plus_i();
            self.stack.push(SynValue::LPlusI(val));
        }

        fn exit_l_plus_i(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 78;
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusI::V1 { num, last_iteration };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::LPlusI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_plus_i(acc, ctx, spans);
        }

        fn exit_rrec(&mut self) {
            let rrec_i = self.stack.pop().unwrap().get_rrec_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxRrec::V1 { id, num, rrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_rrec(ctx, spans);
            self.stack.push(SynValue::Rrec(val));
        }

        fn exit_l_rrec(&mut self) {
            let l_rrec_i = self.stack.pop().unwrap().get_l_rrec_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLRrec::V1 { id, num, l_rrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_rrec(ctx, spans);
            self.stack.push(SynValue::LRrec(val));
        }

        fn exit_lrec(&mut self) {
            let lrec_i = self.stack.pop().unwrap().get_lrec_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLrec::V1 { id, lrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_lrec(ctx, spans);
            self.stack.push(SynValue::Lrec(val));
        }

        fn exit_amb(&mut self) {
            let amb_i = self.stack.pop().unwrap().get_amb_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxAmb::V1 { id, amb_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_amb(ctx, spans);
            self.stack.push(SynValue::Amb(val));
        }

        fn exit_star_a(&mut self) {
            let star = self.stack.pop().unwrap().get_star_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxStarA::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_star_a(ctx, spans);
            self.stack.push(SynValue::StarA(val));
        }

        fn init_star_a1(&mut self) {
            let val = SynStarA1(Vec::new());
            self.stack.push(SynValue::StarA1(val));
        }

        fn exit_star_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                48 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynStarA1Item::V1 { id })
                }
                49 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, SynStarA1Item::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_star_a1"),
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::StarA1(SynStarA1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynStarA1 item on wrapper stack");
            };
            star_acc.push(val);
        }

        fn exit_plus_a(&mut self) {
            let plus = self.stack.pop().unwrap().get_plus_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxPlusA::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_plus_a(ctx, spans);
            self.stack.push(SynValue::PlusA(val));
        }

        fn init_plus_a1(&mut self) {
            let val = SynPlusA1(Vec::new());
            self.stack.push(SynValue::PlusA1(val));
        }

        fn exit_plus_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                87 | 88 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynPlusA1Item::V1 { id })
                }
                89 | 90 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, SynPlusA1Item::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_plus_a1"),
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::PlusA1(SynPlusA1(plus_acc))) = self.stack.last_mut() else {
                panic!("expected SynPlusA1 item on wrapper stack");
            };
            plus_acc.push(val);
        }

        fn exit_l_star_a(&mut self) {
            let star = self.stack.pop().unwrap().get_l_star_a_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLStarA::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_star_a(ctx, spans);
            self.stack.push(SynValue::LStarA(val));
        }

        fn init_l_star_a_i(&mut self) {
            let val = self.listener.init_l_star_a_i();
            self.stack.push(SynValue::LStarAI(val));
        }

        fn exit_l_star_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                31 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLStarAI::V1 { id })
                }
                32 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, CtxLStarAI::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_l_star_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::LStarAI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_star_a_i(acc, ctx, spans);
        }

        fn exitloop_l_star_a_i(&mut self) {
            let SynValue::LStarAI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_l_star_a_i(acc);
        }

        fn exit_l_plus_a(&mut self) {
            let plus = self.stack.pop().unwrap().get_l_plus_a_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusA::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_plus_a(ctx, spans);
            self.stack.push(SynValue::LPlusA(val));
        }

        fn init_l_plus_a_i(&mut self) {
            let val = self.listener.init_l_plus_a_i();
            self.stack.push(SynValue::LPlusAI(val));
        }

        fn exit_l_plus_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                79 | 80 => {
                    let last_iteration = alt_id == 80;
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLPlusAI::V1 { id, last_iteration })
                }
                81 | 82 => {
                    let last_iteration = alt_id == 82;
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, CtxLPlusAI::V2 { num, id, last_iteration })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_l_plus_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::LPlusAI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_plus_a_i(acc, ctx, spans);
        }

        fn exit_sep_list(&mut self) {
            let star = self.stack.pop().unwrap().get_sep_list1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxSepList::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_sep_list(ctx, spans);
            self.stack.push(SynValue::SepList(val));
        }

        fn init_sep_list1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let val = SynSepList1Item { id, num };
            self.stack.push(SynValue::SepList1(SynSepList1(vec![val])));
        }

        fn exit_sep_list1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let val = SynSepList1Item { id, num };
            let Some(SynValue::SepList1(SynSepList1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynSepList1 item on wrapper stack");
            };
            star_acc.push(val);
        }

        fn exit_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                83 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxSepListOpt::V2 { id })
                }
                84 => {
                    let star = self.stack.pop().unwrap().get_sep_list_opt1();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxSepListOpt::V1 { id, star })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_sep_list_opt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_sep_list_opt(ctx, spans);
            self.stack.push(SynValue::SepListOpt(val));
        }

        fn init_sep_list_opt1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let val = SynSepListOpt1Item { id, num };
            self.stack.push(SynValue::SepListOpt1(SynSepListOpt1(vec![val])));
        }

        fn exit_sep_list_opt1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let val = SynSepListOpt1Item { id, num };
            let Some(SynValue::SepListOpt1(SynSepListOpt1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynSepListOpt1 item on wrapper stack");
            };
            star_acc.push(val);
        }

        fn exit_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                39 => {
                    let rrec_i = self.stack.pop().unwrap().get_rrec_i();
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxRrecI::V1 { num, rrec_i })
                }
                40 => {
                    (1, CtxRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_rrec_i(ctx, spans);
            self.stack.push(SynValue::RrecI(val));
        }

        fn init_l_rrec_i(&mut self) {
            let val = self.listener.init_l_rrec_i();
            self.stack.push(SynValue::LRrecI(val));
        }

        fn exit_l_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                41 => {
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxLRrecI::V1 { num })
                }
                42 => {
                    (2, CtxLRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_l_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::LRrecI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_rrec_i(acc, ctx, spans);
        }

        fn inter_lrec_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLrecI::V2 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_lrec_i(ctx, spans);
            self.stack.push(SynValue::LrecI(val));
        }

        fn exit_lrec_i1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let lrec_i = self.stack.pop().unwrap().get_lrec_i();
            let ctx = CtxLrecI::V1 { lrec_i, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_lrec_i(ctx, spans);
            self.stack.push(SynValue::LrecI(val));
        }

        fn exitloop_lrec_i1(&mut self) {
            let SynValue::LrecI(lrec_i) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_lrec_i(lrec_i);
        }

        fn exit_amb_i1(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                59 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V1 { amb_i: [amb_i_1, amb_i_2] })
                }
                60 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V2 { amb_i: [amb_i_1, amb_i_2] })
                }
                61 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V3 { amb_i: [amb_i_1, amb_i_2] })
                }
                62 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V4 { amb_i: [amb_i_1, amb_i_2] })
                }
                63 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V5 { amb_i: [amb_i_1, amb_i_2] })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_amb_i1")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_amb_i(ctx, spans);
            self.stack.push(SynValue::AmbI(val));
        }

        fn exit_amb_i6(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                73 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (2, CtxAmbI::V6 { amb_i })
                }
                74 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V7 { amb_i })
                }
                75 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V8 { id })
                }
                76 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V9 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_amb_i6")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_amb_i(ctx, spans);
            self.stack.push(SynValue::AmbI(val));
        }
    }

    // [pandemonium_parser]
}
