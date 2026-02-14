// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

mod value;
mod no_value;

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
    r#"exit_amb_i("-", "6")"#,
    r#"exit_amb_i("2", "*", "-6")"#,
    r#"exit_amb_i("5", "-", "2*-6")"#,
    r#"exit_amb_i("3")"#,
    r#"exit_amb_i("2")"#,
    r#"exit_amb_i("4")"#,
    r#"exit_amb_i("2", "^", "4")"#,
    r#"exit_amb_i("3", "^", "2^4")"#,
    r#"exit_amb_i("81")"#,
    r#"exit_amb_i("3^2^4", "/", "81")"#,
    r#"exit_amb_i("5 - 2*-6", "+", "3^2^4 / 81")"#,
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
// test helper

#[allow(unused)]
mod level_string {
    use std::cmp::max;

    #[derive(Debug, PartialEq)]
    pub struct LevelString(pub u32, pub String);

    impl LevelString {
        pub fn get_string(self) -> String {
            self.1
        }
    }

    pub fn par(ls: LevelString) -> String {
        if ls.0 > 0 {
            format!("({})", ls.1)
        } else {
            ls.1
        }
    }

    pub fn ls_prefix_op(op: &str, ls: LevelString) -> LevelString {
        LevelString(ls.0 + 1, format!("{op} {}", par(ls)))
    }

    pub fn ls_suffix_op(op: &str, ls: LevelString) -> LevelString {
        LevelString(ls.0 + 1, format!("{} {op}", par(ls)))
    }

    pub fn ls_binary_op(op: &str, lsleft: LevelString, lsright: LevelString) -> LevelString {
        LevelString(max(lsleft.0, lsright.0) + 1, format!("{} {op} {}", par(lsleft), par(lsright)))
    }
}
