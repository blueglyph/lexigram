use crate::build::BuildFrom;
use crate::grammar::ProdRuleSet;
use crate::grammar::tests::prs::test_prs_transforms;
use crate::LR;

#[test]
fn prs_lr_from() {
    let tests = vec![
        //#[cfg(any())]
        (14, vec![
            // a -> b c | c
            // b -> Op c
            // c -> Id
            r#"a -> b c | c"#,                                  //
            r#"b -> Op c"#,                                     //
            r#"c -> Id"#,                                       //
            r#"<goal> -> a"#,                                   //
        ], vec![0, 0, 0, 0], vec![None, None, None, None]),
        //#[cfg(any())]
        (552, vec![
            // e -> e "-" t | t
            // t -> Id | "(" e ")"
            r#"e -> e "-" t | t"#,                              //
            r#"t -> Id | "(" e ")""#,                           //
            r#"<goal> -> e"#,                                   //
        ], vec![0, 0, 0], vec![None, None, None]),
        (2000, vec![
            // s -> "a" a "a" | "a" "a" "b" | "b" a "b"
            // a -> b c
            // b -> "a"
            // c -> d
            r#"s -> "a" a "a" | "a" "a" "b" | "b" a "b""#,      //
            r#"a -> b c"#,                                      //
            r#"b -> "a""#,                                      //
            r#"c -> d"#,                                        //
            r#"d -> <empty>"#,                                  //
            r#"<goal> -> s"#,                                   //
        ], vec![0, 0, 0, 0, 0, 0], vec![None, None, None, None, None, None]),
        /* template:
        (1, vec![
        ], vec![], vec![]),
        */
    ];
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;

    test_prs_transforms(
        tests
            // .into_iter().filter(|t| matches!(t.0, 2000)).collect()
        ,
        |prs| {
            if VERBOSE { prs.symbol_table.as_ref().unwrap().dump("Symbol table"); }
            let mut lr = ProdRuleSet::<LR>::build_from(prs);
            match lr.make_parsing_table_lalr(false) {
                Ok(table) => {
                    if VERBOSE {
                        println!("Table:\n{}", table.to_str(lr.get_symbol_table()).join("\n"));
                    }
                }
                Err(_) => {}
            }
            lr
        },
        VERBOSE, SHOW_ANSWER_ONLY, true);
}
