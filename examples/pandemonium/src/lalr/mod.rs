// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

mod value;
mod no_value;

/// Text to parse
pub static TXT1: &str = r#"
star    Alpha   = a, 101, 110, 150;
plus    Bravo   = 102, 120, 250;
l-star  Charlie = 103, 130, 350;
l-plus  Delta   = 104, 140, 450;
rrec    Echo    = 105, 150, 550;
lrec    Golf    = 107, 170, 750;
amb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;

star-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];
plus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];
l-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];
l-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];

star    Mike     = x;
l-star  November = 202;
rrec    Oscar    = 203;
lrec    Quebec   = 205;

sep-list     Romeo   = a:1, then b: 2, then c:3;
sep-list     Sierra  = d: 4;
sep-list-opt Tango   = e: 5, then f:6, then g: 7;
sep-list-opt Uniform =;

l-sep-list     Victor  = a:1, then b: 2, then c:3;
l-sep-list     Whiskey = d: 4;
l-sep-list-opt Xray    = e: 5, then f:6, then g: 7;
l-sep-list-opt Yankee  =;
;
"#;
/// Expected spans collected when parsing TXT1
pub static SPANS1: &[&str] = &[
    r#"exit_star("Alpha", "=", "a", ", 101, 110, 150", ";")"#,
    r#"exit_example("star", "Alpha   = a, 101, 110, 150;")"#,
    r#"exit_i("", "star    Alpha   = a, 101, 110, 150;")"#,
    r#"exit_plus("Bravo", "=", "102", ", 120, 250", ";")"#,
    r#"exit_example("plus", "Bravo   = 102, 120, 250;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;", "plus    Bravo   = 102, 120, 250;")"#,
    r#"exit_l_star_i("", ",", "130")"#,
    r#"exit_l_star_i(", 130", ",", "350")"#,
    r#"exit_l_star("Charlie", "=", "103", ", 130, 350", ";")"#,
    r#"exit_example("l-star", "Charlie = 103, 130, 350;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;", "l-star  Charlie = 103, 130, 350;")"#,
    r#"exit_l_plus_i("", ",", "140")"#,
    r#"exit_l_plus_i(", 140", ",", "450")"#,
    r#"exit_l_plus("Delta", "=", "104", ", 140, 450", ";")"#,
    r#"exit_example("l-plus", "Delta   = 104, 140, 450;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;", "l-plus  Delta   = 104, 140, 450;")"#,
    r#"exit_rrec_i(";")"#,
    r#"exit_rrec_i(",", "550", ";")"#,
    r#"exit_rrec_i(",", "150", ", 550;")"#,
    r#"exit_rrec("Echo", "=", "105", ", 150, 550;")"#,
    r#"exit_example("rrec", "Echo    = 105, 150, 550;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;", "rrec    Echo    = 105, 150, 550;")"#,
    r#"exit_lrec_i("107")"#,
    r#"exit_lrec_i("107", ",", "170")"#,
    r#"exit_lrec_i("107, 170", ",", "750")"#,
    r#"exit_lrec("Golf", "=", "107, 170, 750", ";")"#,
    r#"exit_example("lrec", "Golf    = 107, 170, 750;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;", "lrec    Golf    = 107, 170, 750;")"#,
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
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;", "amb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;")"#,

    r#"exit_star_a("India", "=", "[", "1:Alpha Beta 4:Delta Echo 10:Juliet", "]", ";")"#,
    r#"exit_example("star-a", "India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;", "star-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];")"#,
    r#"exit_plus_a("Juliet", "=", "[", "11:Kilo Lima Mike 26:Zoulou", "]", ";")"#,
    r#"exit_example("plus-a", "Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];", "plus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];")"#,
    r#"exit_l_star_a_i("", "2", ":", "Beta")"#,
    r#"exit_l_star_a_i("2:Beta", "Charlie")"#,
    r#"exit_l_star_a_i("2:Beta Charlie", "5", ":", "Echo")"#,
    r#"exit_l_star_a("Kilo", "=", "[", "2:Beta Charlie 5:Echo", "]", ";")"#,
    r#"exit_example("l-star-a", "Kilo   = [ 2:Beta Charlie 5:Echo ];")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];", "l-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];")"#,
    r#"exit_l_plus_a_i("", "21", ":", "Uniform")"#,
    r#"exit_l_plus_a_i("21:Uniform", "Victor")"#,
    r#"exit_l_plus_a_i("21:Uniform Victor", "25", ":", "Yankee")"#,
    r#"exit_l_plus_a("Lima", "=", "[", "21:Uniform Victor 25:Yankee", "]", ";")"#,
    r#"exit_example("l-plus-a", "Lima   = [ 21:Uniform Victor 25:Yankee ];")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];", "l-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];")"#,

    r#"exit_star("Mike", "=", "x", "", ";")"#,
    r#"exit_example("star", "Mike     = x;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];", "star    Mike     = x;")"#,
    r#"exit_l_star("November", "=", "202", "", ";")"#,
    r#"exit_example("l-star", "November = 202;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;", "l-star  November = 202;")"#,
    r#"exit_rrec_i(";")"#,
    r#"exit_rrec("Oscar", "=", "203", ";")"#,
    r#"exit_example("rrec", "Oscar    = 203;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;", "rrec    Oscar    = 203;")"#,
    r#"exit_lrec_i("205")"#,
    r#"exit_lrec("Quebec", "=", "205", ";")"#,
    r#"exit_example("lrec", "Quebec   = 205;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;", "lrec    Quebec   = 205;")"#,

    r#"exit_sep_list("Romeo", "=", "a:1, then b: 2, then c:3", ";")"#,
    r#"exit_example("sep-list", "Romeo   = a:1, then b: 2, then c:3;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;", "sep-list     Romeo   = a:1, then b: 2, then c:3;")"#,
    r#"exit_sep_list("Sierra", "=", "d: 4", ";")"#,
    r#"exit_example("sep-list", "Sierra  = d: 4;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;", "sep-list     Sierra  = d: 4;")"#,
    r#"exit_sep_list_opt("Tango", "=", "e: 5, then f:6, then g: 7", ";")"#,
    r#"exit_example("sep-list-opt", "Tango   = e: 5, then f:6, then g: 7;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;", "sep-list-opt Tango   = e: 5, then f:6, then g: 7;")"#,
    r#"exit_sep_list_opt("Uniform", "=", ";")"#,
    r#"exit_example("sep-list-opt", "Uniform =;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;\nsep-list-opt Tango   = e: 5, then f:6, then g: 7;", "sep-list-opt Uniform =;")"#,

    r#"exit_l_sep_list_i("a", ":", "1")"#,
    r#"exit_l_sep_list_i("b", ":", "2")"#,
    r#"exit_l_sep_list_i("c", ":", "3")"#,
    r#"exit_l_sep_list("Victor", "=", "a:1, then b: 2, then c:3", ";")"#,
    r#"exit_example("l-sep-list", "Victor  = a:1, then b: 2, then c:3;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;\nsep-list-opt Tango   = e: 5, then f:6, then g: 7;\nsep-list-opt Uniform =;", "l-sep-list     Victor  = a:1, then b: 2, then c:3;")"#,
    r#"exit_l_sep_list_i("d", ":", "4")"#,
    r#"exit_l_sep_list("Whiskey", "=", "d: 4", ";")"#,
    r#"exit_example("l-sep-list", "Whiskey = d: 4;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;\nsep-list-opt Tango   = e: 5, then f:6, then g: 7;\nsep-list-opt Uniform =;\n\nl-sep-list     Victor  = a:1, then b: 2, then c:3;", "l-sep-list     Whiskey = d: 4;")"#,

    r#"exit_l_sep_list_opt_i("e", ":", "5")"#,
    r#"exit_l_sep_list_opt_i("f", ":", "6")"#,
    r#"exit_l_sep_list_opt_i("g", ":", "7")"#,
    r#"exit_l_sep_list_opt("Xray", "=", "e: 5, then f:6, then g: 7", ";")"#,
    r#"exit_example("l-sep-list-opt", "Xray    = e: 5, then f:6, then g: 7;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;\nsep-list-opt Tango   = e: 5, then f:6, then g: 7;\nsep-list-opt Uniform =;\n\nl-sep-list     Victor  = a:1, then b: 2, then c:3;\nl-sep-list     Whiskey = d: 4;", "l-sep-list-opt Xray    = e: 5, then f:6, then g: 7;")"#,
    r#"exit_l_sep_list_opt("Yankee", "=", ";")"#,
    r#"exit_example("l-sep-list-opt", "Yankee  =;")"#,
    r#"exit_i("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;\nsep-list-opt Tango   = e: 5, then f:6, then g: 7;\nsep-list-opt Uniform =;\n\nl-sep-list     Victor  = a:1, then b: 2, then c:3;\nl-sep-list     Whiskey = d: 4;\nl-sep-list-opt Xray    = e: 5, then f:6, then g: 7;", "l-sep-list-opt Yankee  =;")"#,

    r#"exit_text("star    Alpha   = a, 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = x;\nl-star  November = 202;\nrrec    Oscar    = 203;\nlrec    Quebec   = 205;\n\nsep-list     Romeo   = a:1, then b: 2, then c:3;\nsep-list     Sierra  = d: 4;\nsep-list-opt Tango   = e: 5, then f:6, then g: 7;\nsep-list-opt Uniform =;\n\nl-sep-list     Victor  = a:1, then b: 2, then c:3;\nl-sep-list     Whiskey = d: 4;\nl-sep-list-opt Xray    = e: 5, then f:6, then g: 7;\nl-sep-list-opt Yankee  =;", ";", "")"#,
];
/// Text to parser (no-values)
pub static TXT2: &str = r#"
;
star    Alpha   = +, *, *;
plus    Bravo   = +, *, *, *;
l-star  Charlie = +, *, *, *, *;
l-plus  Delta   = +, *, *, *;
rrec    Echo    = +, *, *;
lrec    Golf    = +, *, *;

star-a   India  = [ *- + *- ];
plus-a   Juliet = [ *- + *- ];
l-star-a Kilo   = [ *- + *- + *- ];
l-plus-a Lima   = [ *- + *- + *- ];

star    Mike     = +;
l-star  November = +;
rrec    Oscar    = +;
lrec    Quebec   = +;

sep-list       Romeo   = *, then *, then *;
sep-list       Sierra  = *;
sep-list-opt   Tango   = *, then *;
sep-list-opt   Uniform =;
l-sep-list     Victor  = *, then *, then *;
l-sep-list     Whiskey = *;
l-sep-list-opt Xray    = *, then *;
l-sep-list-opt Yankee  =;
"#;
/// Expected spans collected when parsing TXT2
pub static SPANS2: &[&str] = &[
    r#"exit_nv_star("Alpha", "=", "+", ", *, *", ";")"#,
    r#"exit_nv_example("star", "Alpha   = +, *, *;")"#,
    r#"exit_nv_i("", "star    Alpha   = +, *, *;")"#,
    r#"exit_nv_plus("Bravo", "=", "+", ", *, *, *", ";")"#,
    r#"exit_nv_example("plus", "Bravo   = +, *, *, *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;", "plus    Bravo   = +, *, *, *;")"#,
    r#"exit_nv_l_star_i("", ",", "*")"#,
    r#"exit_nv_l_star_i(", *", ",", "*")"#,
    r#"exit_nv_l_star_i(", *, *", ",", "*")"#,
    r#"exit_nv_l_star_i(", *, *, *", ",", "*")"#,
    r#"exit_nv_l_star("Charlie", "=", "+", ", *, *, *, *", ";")"#,
    r#"exit_nv_example("l-star", "Charlie = +, *, *, *, *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;", "l-star  Charlie = +, *, *, *, *;")"#,
    r#"exit_nv_l_plus_i("", ",", "*")"#,
    r#"exit_nv_l_plus_i(", *", ",", "*")"#,
    r#"exit_nv_l_plus_i(", *, *", ",", "*")"#,
    r#"exit_nv_l_plus("Delta", "=", "+", ", *, *, *", ";")"#,
    r#"exit_nv_example("l-plus", "Delta   = +, *, *, *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;", "l-plus  Delta   = +, *, *, *;")"#,
    r#"exit_nv_rrec_i(";")"#,
    r#"exit_nv_rrec_i(",", "*", ";")"#,
    r#"exit_nv_rrec_i(",", "*", ", *;")"#,
    r#"exit_nv_rrec("Echo", "=", "+", ", *, *;")"#,
    r#"exit_nv_example("rrec", "Echo    = +, *, *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;", "rrec    Echo    = +, *, *;")"#,
    r#"exit_nv_lrec_i("+")"#,
    r#"exit_nv_lrec_i("+", ",", "*")"#,
    r#"exit_nv_lrec_i("+, *", ",", "*")"#,
    r#"exit_nv_lrec("Golf", "=", "+, *, *", ";")"#,
    r#"exit_nv_example("lrec", "Golf    = +, *, *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;", "lrec    Golf    = +, *, *;")"#,

    r#"exit_nv_star_a("India", "=", "[", "*- + *-", "]", ";")"#,
    r#"exit_nv_example("star-a", "India  = [ *- + *- ];")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;", "star-a   India  = [ *- + *- ];")"#,
    r#"exit_nv_plus_a("Juliet", "=", "[", "*- + *-", "]", ";")"#,
    r#"exit_nv_example("plus-a", "Juliet = [ *- + *- ];")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];", "plus-a   Juliet = [ *- + *- ];")"#,
    r#"exit_nv_l_star_a_i("", "*", "-")"#,
    r#"exit_nv_l_star_a_i("*-", "+")"#,
    r#"exit_nv_l_star_a_i("*- +", "*", "-")"#,
    r#"exit_nv_l_star_a_i("*- + *-", "+")"#,
    r#"exit_nv_l_star_a_i("*- + *- +", "*", "-")"#,
    r#"exit_nv_l_star_a("Kilo", "=", "[", "*- + *- + *-", "]", ";")"#,
    r#"exit_nv_example("l-star-a", "Kilo   = [ *- + *- + *- ];")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];", "l-star-a Kilo   = [ *- + *- + *- ];")"#,
    r#"exit_nv_l_plus_a_i("", "*", "-")"#,
    r#"exit_nv_l_plus_a_i("*-", "+")"#,
    r#"exit_nv_l_plus_a_i("*- +", "*", "-")"#,
    r#"exit_nv_l_plus_a_i("*- + *-", "+")"#,
    r#"exit_nv_l_plus_a_i("*- + *- +", "*", "-")"#,
    r#"exit_nv_l_plus_a("Lima", "=", "[", "*- + *- + *-", "]", ";")"#,
    r#"exit_nv_example("l-plus-a", "Lima   = [ *- + *- + *- ];")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];", "l-plus-a Lima   = [ *- + *- + *- ];")"#,

    r#"exit_nv_star("Mike", "=", "+", "", ";")"#,
    r#"exit_nv_example("star", "Mike     = +;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];", "star    Mike     = +;")"#,
    r#"exit_nv_l_star("November", "=", "+", "", ";")"#,
    r#"exit_nv_example("l-star", "November = +;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;", "l-star  November = +;")"#,
    r#"exit_nv_rrec_i(";")"#,
    r#"exit_nv_rrec("Oscar", "=", "+", ";")"#,
    r#"exit_nv_example("rrec", "Oscar    = +;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;", "rrec    Oscar    = +;")"#,
    r#"exit_nv_lrec_i("+")"#,
    r#"exit_nv_lrec("Quebec", "=", "+", ";")"#,
    r#"exit_nv_example("lrec", "Quebec   = +;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;", "lrec    Quebec   = +;")"#,

    r#"exit_nv_sep_list("Romeo", "=", "*, then *, then *", ";")"#,
    r#"exit_nv_example("sep-list", "Romeo   = *, then *, then *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;", "sep-list       Romeo   = *, then *, then *;")"#,
    r#"exit_nv_sep_list("Sierra", "=", "*", ";")"#,
    r#"exit_nv_example("sep-list", "Sierra  = *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;\n\nsep-list       Romeo   = *, then *, then *;", "sep-list       Sierra  = *;")"#,
    r#"exit_nv_sep_list_opt("Tango", "=", "*, then *", ";")"#,
    r#"exit_nv_example("sep-list-opt", "Tango   = *, then *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;\n\nsep-list       Romeo   = *, then *, then *;\nsep-list       Sierra  = *;", "sep-list-opt   Tango   = *, then *;")"#,
    r#"exit_nv_sep_list_opt("Uniform", "=", ";")"#,
    r#"exit_nv_example("sep-list-opt", "Uniform =;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;\n\nsep-list       Romeo   = *, then *, then *;\nsep-list       Sierra  = *;\nsep-list-opt   Tango   = *, then *;", "sep-list-opt   Uniform =;")"#,
    r#"exit_nv_l_sep_list_i("*")"#,
    r#"exit_nv_l_sep_list_i("*")"#,
    r#"exit_nv_l_sep_list_i("*")"#,
    r#"exit_nv_l_sep_list("Victor", "=", "*, then *, then *", ";")"#,
    r#"exit_nv_example("l-sep-list", "Victor  = *, then *, then *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;\n\nsep-list       Romeo   = *, then *, then *;\nsep-list       Sierra  = *;\nsep-list-opt   Tango   = *, then *;\nsep-list-opt   Uniform =;", "l-sep-list     Victor  = *, then *, then *;")"#,
    r#"exit_nv_l_sep_list_i("*")"#,
    r#"exit_nv_l_sep_list("Whiskey", "=", "*", ";")"#,
    r#"exit_nv_example("l-sep-list", "Whiskey = *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;\n\nsep-list       Romeo   = *, then *, then *;\nsep-list       Sierra  = *;\nsep-list-opt   Tango   = *, then *;\nsep-list-opt   Uniform =;\nl-sep-list     Victor  = *, then *, then *;", "l-sep-list     Whiskey = *;")"#,
    r#"exit_nv_l_sep_list_opt_i("*")"#,
    r#"exit_nv_l_sep_list_opt_i("*")"#,
    r#"exit_nv_l_sep_list_opt("Xray", "=", "*, then *", ";")"#,
    r#"exit_nv_example("l-sep-list-opt", "Xray    = *, then *;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;\n\nsep-list       Romeo   = *, then *, then *;\nsep-list       Sierra  = *;\nsep-list-opt   Tango   = *, then *;\nsep-list-opt   Uniform =;\nl-sep-list     Victor  = *, then *, then *;\nl-sep-list     Whiskey = *;", "l-sep-list-opt Xray    = *, then *;")"#,
    r#"exit_nv_l_sep_list_opt("Yankee", "=", ";")"#,
    r#"exit_nv_example("l-sep-list-opt", "Yankee  =;")"#,
    r#"exit_nv_i("star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;\n\nsep-list       Romeo   = *, then *, then *;\nsep-list       Sierra  = *;\nsep-list-opt   Tango   = *, then *;\nsep-list-opt   Uniform =;\nl-sep-list     Victor  = *, then *, then *;\nl-sep-list     Whiskey = *;\nl-sep-list-opt Xray    = *, then *;", "l-sep-list-opt Yankee  =;")"#,
    r#"exit_text("", ";", "star    Alpha   = +, *, *;\nplus    Bravo   = +, *, *, *;\nl-star  Charlie = +, *, *, *, *;\nl-plus  Delta   = +, *, *, *;\nrrec    Echo    = +, *, *;\nlrec    Golf    = +, *, *;\n\nstar-a   India  = [ *- + *- ];\nplus-a   Juliet = [ *- + *- ];\nl-star-a Kilo   = [ *- + *- + *- ];\nl-plus-a Lima   = [ *- + *- + *- ];\n\nstar    Mike     = +;\nl-star  November = +;\nrrec    Oscar    = +;\nlrec    Quebec   = +;\n\nsep-list       Romeo   = *, then *, then *;\nsep-list       Sierra  = *;\nsep-list-opt   Tango   = *, then *;\nsep-list-opt   Uniform =;\nl-sep-list     Victor  = *, then *, then *;\nl-sep-list     Whiskey = *;\nl-sep-list-opt Xray    = *, then *;\nl-sep-list-opt Yankee  =;")"#,
];
