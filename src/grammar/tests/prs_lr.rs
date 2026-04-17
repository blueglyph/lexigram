use crate::build::BuildFrom;
use crate::grammar::ProdRuleSet;
use crate::grammar::tests::prs::test_prs_transforms;
use crate::LR;

#[test]
fn prs_lr_from() {
    let tests = vec![
        (14, vec![
            // a -> b c | c
            // b -> Op c
            // c -> Id
            r#"a -> b c | c"#,
            r#"b -> Op c"#,
            r#"c -> Id"#,
        ], vec![0, 0, 0], vec![None, None, None]),
        /* template:
        (1, vec![
        ], vec![], vec![]),
        */
    ];
    const VERBOSE: bool = true;
    const SHOW_ANSWER_ONLY: bool = false;

    test_prs_transforms(
        tests,
        |prs| {
            if VERBOSE { prs.symbol_table.as_ref().unwrap().dump("Symbol table"); }
            ProdRuleSet::<LR>::build_from(prs)
        },
        VERBOSE, SHOW_ANSWER_ONLY, true);
}
