use crate::build::BuildFrom;
use crate::grammar::ProdRuleSet;
use crate::grammar::tests::prs::test_prs_transforms;
use crate::LR;

#[test]
fn prs_lr_from() {
    let tests = vec![
        #[cfg(any())]
        (14, vec![
            // a -> b c | c
            // b -> Op c
            // c -> Id
            r#"a -> b c | c"#,
            r#"b -> Op c"#,
            r#"c -> Id"#,
        ], vec![0, 0, 0], vec![None, None, None]),
        #[cfg(any())]
        (552, vec![
            // e -> e "-" t | t
            // t -> Id | "(" e ")"
            r#"e -> e "-" t | t"#,                              //
            r#"t -> Id | "(" e ")""#,                           //
        ], vec![0, 0], vec![None, None]),
        (2000, vec![
        ], vec![], vec![]),
        /* template:
        (1, vec![
        ], vec![], vec![]),
        */
    ];
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;

    test_prs_transforms(
        tests,
        |prs| {
            if VERBOSE { prs.symbol_table.as_ref().unwrap().dump("Symbol table"); }
            let mut lr = ProdRuleSet::<LR>::build_from(prs);
            let _ = lr.make_parsing_table_lalr(false);
            lr
        },
        VERBOSE, SHOW_ANSWER_ONLY, true);
}
