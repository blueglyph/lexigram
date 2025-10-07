// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use crate::grammar::tests::old_build_rts_prs;
use crate::log::BuildFrom;
use super::*;

// ---------------------------------------------------------------------------------------------
// ProdRuleSet

impl<T> ProdRuleSet<T> {
    pub fn get_non_empty_nts(&self) -> impl Iterator<Item=(VarId, &ProdRule)> {
        self.get_prules_iter().filter(|(_, rule)| **rule != prule!(e))
    }
}

impl<T> ProdRuleSet<T> {
    pub fn new() -> Self {
        Self {
            prules: Vec::new(),
            origin: Origin::new(),
            num_nt: 0,
            num_t: 0,
            symbol_table: None,
            flags: Vec::new(),
            parent: Vec::new(),
            start: None,
            name: None,
            nt_conversion: HashMap::new(),
            log: BufLog::new(),
            _phantom: PhantomData
        }
    }

    pub(crate) fn print_prs_summary(&self) {
        let alts = self.get_alts().map(|(v, f)| (v, f.clone())).collect::<Vec<_>>();
        print_alts(&alts, self.get_symbol_table());
        let nt_flags = self.flags.iter().index().filter_map(|(nt, &f)|
            if f != 0 { Some(format!("  - NT[{nt}] {}: {} ({})", Symbol::NT(nt).to_str(self.get_symbol_table()), ruleflag::to_string(f).join(" | "), f)) } else { None }
        ).join("\n");
        let parents = self.parent.iter().index().filter_map(|(c, &par)|
            par.map(|p| format!("  - {} -> {}", Symbol::NT(c).to_str(self.get_symbol_table()), Symbol::NT(p).to_str(self.get_symbol_table())))
        ).join("\n");
        println!("- NT flags:\n{}", if nt_flags.is_empty() { "  - (nothing)".to_string() } else { nt_flags });
        println!("- parents:\n{}", if parents.is_empty() { "  - (nothing)".to_string() } else { parents });
    }
}

impl<T> From<&ProdRuleSet<T>> for BTreeMap<VarId, ProdRule> {
    fn from(rules: &ProdRuleSet<T>) -> Self {
        rules.get_prules_iter().map(|(var, p)| (var, p.clone())).collect::<BTreeMap<_, _>>()

    }
}

pub fn print_alts<T: SymInfoTable>(alts: &Vec<(VarId, Alternative)>, symbol_table: Option<&T>) {
    println!("{}",
             alts.iter().enumerate().map(|(id, (v, f))|
                 format!("            // - {id}: {} -> {}{}",
                         Symbol::NT(*v).to_str(symbol_table),
                         f.to_str(symbol_table),
                         if f.get_flags() != 0 { format!("     {} ({})", ruleflag::to_string(f.get_flags()).join(" | "), f.get_flags()) } else { "".to_string() }
                 )
    ).join("\n"));
}

#[allow(unused)]
pub fn print_expected_code(result: &BTreeMap<VarId, ProdRule>) {
    println!("            {}", result.iter().map(|(i, p)|
        format!("{i} => prule!({}),", p.iter()
            .map(|f| format!("{}{}", if f.get_flags() != 0 { format!("#{}, ", f.get_flags()) } else { "".to_string() }, f.iter().map(|s| s.to_macro_item()).join(", ")))
            .join("; "))).join("\n            "))
}

// ---------------------------------------------------------------------------------------------

fn test_prs_transforms<F, T>(
    tests: Vec<(u32, Vec<&str>, Vec<u32>, Vec<Option<VarId>>)>,
    mut f: F,
    verbose: bool, show_answer_only: bool, comment_original_rules: bool)
where
    F: FnMut(ProdRuleSet<General>) -> ProdRuleSet<T>
{
    let mut errors = 0;
    for (test_id, expected, expected_flags, expected_parent) in tests {
        let rts = TestRules(test_id).to_rts_general().unwrap();
        let symtab = rts.get_symbol_table();
        let original_rules = rts.get_non_empty_nts()
                .map(|(v, t)| format!("            // {} -> {}", Symbol::NT(v).to_str(symtab), grtree_to_str(t, None, None, symtab, false)))
                .join("\n");
        if verbose && !show_answer_only {
            println!("{:=<80}\ntest {test_id}:", "");
            if !comment_original_rules {
                println!("Original rules:\n{original_rules}");
            }
        }
        let prs = ProdRuleSet::build_from(rts);
        assert!(prs.log.has_no_errors(), "test {test_id} failed to create production rules:\n{}", prs.log.get_messages_str());
        let prs = f(prs);
        let symtab = prs.get_symbol_table();
        let result = prs.get_prules_iter().map(|(id, p)| prule_to_rule_str(id, &p, symtab)).to_vec();
        let num_vars = result.len();
        if verbose || show_answer_only {
            let flags = (0..num_vars).into_iter().map(|nt| prs.flags[nt]).join(", ");
            let parents = (0..num_vars).into_iter().map(|nt| format!("{:?}", prs.parent[nt])).join(", ");
            let comment_flags = (0..num_vars).into_iter().map(|nt| format!(" // {}", ruleflag::to_string(prs.flags[nt]).join(" | "))).to_vec();
            let lines = result.iter().zip(comment_flags).map(|(s1, s2)| vec![format!("r#\"{s1}\"#,"), s2]).to_vec();
            let cols = columns_to_str(lines, Some(vec![51, 0]));
            if !show_answer_only { println!("Code:"); }
            println!("        ({test_id}, vec![");
            if comment_original_rules {
                println!("{original_rules}");
            }
            println!("{}", cols.into_iter().map(|s| format!("            {s}")).join("\n"));
            println!("        ], vec![{flags}], vec![{parents}]),");
            if !show_answer_only && !prs.log.is_empty() {
                println!("Messages:\n{}", prs.log);
            }
        }
        // let fail1 = result != expected;
        let fail1 = result != expected.into_iter().map(|s| s.to_string()).to_vec();
        let fail2 = prs.flags[..num_vars] != expected_flags;
        let fail3 = prs.parent[..num_vars] != expected_parent;
        let fail4 = !prs.log.has_no_errors();
        if  fail1 || fail2 || fail3 || fail4 {
            errors += 1;
            if !show_answer_only {
                let msg = format!("## ERROR ## test {test_id}: ");
                if fail1 { println!("{msg}rules don't match"); }
                if fail2 { println!("{msg}flags don't match"); }
                if fail3 { println!("{msg}parents don't match"); }
                if fail4 { println!("{msg}errors in log"); }
            }
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}

#[test]
fn rts_prodrule_from() {
    let tests = vec![
        //  rules                                               flags
        // --------------------------------------------------------------------------------
        (1, vec![
            r#"a -> A B"#,                                      //
        ], vec![0], vec![None]),
        (4, vec![
            r#"a -> A B | C D"#,                                //
        ], vec![0], vec![None]),
        (8, vec![
            r#"a -> A B | B"#,                                  //
        ], vec![0], vec![None]),
        (11, vec![
            r#"a -> A b"#,                                      //
            r#"b -> B"#,                                        //
        ], vec![0, 0], vec![None, None]),
        (100, vec![
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (101, vec![
            r#"a -> a_1"#,                                      // parent_+_or_* | plus
            r#"a_1 -> A a_1 | A"#,                              // child_+_or_* | plus
        ], vec![6144, 4097], vec![None, Some(0)]),
        (105, vec![
            r#"a -> a_1"#,                                      // parent_+_or_* | plus
            r#"a_1 -> A B a_1 | A B"#,                          // child_+_or_* | plus
        ], vec![6144, 4097], vec![None, Some(0)]),
        (106, vec![
            r#"a -> a_2 C"#,                                    // parent_+_or_*
            r#"b -> B"#,                                        //
            r#"a_1 -> b "," a_1 | ε"#,                          // child_+_or_*
            r#"a_2 -> A a_1 ";" a_2 | ε"#,                      // child_+_or_* | parent_+_or_*
        ], vec![2048, 0, 1, 2049], vec![None, None, Some(3), Some(0)]),
        (150, vec![
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_1 | B a_1 | ε"#,                      // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (208, vec![
            r#"a -> i C"#,                                      // parent_+_or_*
            r#"i -> A j ";" i | ε"#,                            // child_+_or_* | L-form | parent_+_or_*
            r#"j -> b "," j | ε"#,                              // child_+_or_* | L-form
            r#"b -> B"#,                                        //
        ], vec![2048, 2177, 129, 0], vec![None, Some(0), Some(1), None]),
        /* template:
        (1, vec![
        ], vec![], vec![]),
        */
    ];
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;

    test_prs_transforms(
        tests,
        |mut prs| {
            prs.simplify();
            prs
        },
        VERBOSE, SHOW_ANSWER_ONLY, false);
}

#[test]
fn prs_remove_recursion() {
    let tests = vec![
        (500, vec![
            // a -> a "!" | "?"
            r#"a -> "?" a_1"#,                                  // parent_left_rec
            r#"a_1 -> "!" a_1 | ε"#,                            // child_left_rec
        ], vec![512, 4], vec![None, Some(0)]),
        (600, vec![
            // e -> e "+" e | Num
            r#"e -> e_2 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> "+" e_2 e_1 | ε"#,                        // child_left_rec
            r#"e_2 -> Num"#,                                    //
        ], vec![1536, 4, 0], vec![None, Some(0), Some(0)]),
        // ----- swapping independent terms shouldn't have an impact
        (601, vec![
            // e -> e "*" e | e "+" e | Num | Id
            r#"e -> e_4 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "+" e_2 e_1 | ε"#,          // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                    // child_left_rec
            r#"e_4 -> Num | Id"#,                               //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (602, vec![
            // e -> Num | e "*" e | Id | e "+" e
            r#"e -> e_4 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "+" e_2 e_1 | ε"#,          // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                    // child_left_rec
            r#"e_4 -> Num | Id"#,                               //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        // ----- prefix op; check differences in e_4:
        (603, vec![
            // e -> e "*" e | e "+" e | "!" e | Num
            r#"e -> e_4 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> <G> "*" e_4 e_1 | <G> "+" e_2 e_1 | ε"#,  // child_left_rec       // TODO: check if <G> necessary on * +
            r#"e_2 -> e_4 e_3"#,                                // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                    // child_left_rec
            r#"e_4 -> "!" e | Num"#,                            // right_rec
        ], vec![1536, 4, 512, 4, 2], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (604, vec![
            // e -> e "*" e | "!" e | e "+" e | Num
            r#"e -> e_4 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "+" e_2 e_1 | ε"#,          // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                    // child_left_rec
            r#"e_4 -> "!" e_2 | Num"#,                          // right_rec
        ], vec![1536, 4, 512, 4, 2], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (605, vec![
            // e -> "!" e | e "*" e | e "+" e | Num
            r#"e -> e_4 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "+" e_2 e_1 | ε"#,          // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                    // child_left_rec
            r#"e_4 -> "!" e_4 | Num"#,                          // right_rec
        ], vec![1536, 4, 512, 4, 2], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        // ----- right-associative op
        (606, vec![
            // e -> e "*" e | e "+" e | <R> e "!" e | Num
            r#"e -> e_4 e_1"#,                                                    // parent_left_rec | parent_amb
            r#"e_1 -> <G> "*" e_4 e_1 | <G> "+" e_2 e_1 | <R,G> "!" e e_1 | ε"#,  // child_left_rec     // TODO: check if <G> necessary on * + !
            r#"e_2 -> e_4 e_3"#,                                                  // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                                      // child_left_rec
            r#"e_4 -> Num"#,                                                      //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (607, vec![
            // e -> e "*" e | <R> e "!" e | e "+" e | Num
            r#"e -> e_4 e_1"#,                                            // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | <R> "!" e_2 e_1 | "+" e_2 e_1 | ε"#,  // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                          // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | <R,G> "!" e_2 e_3 | ε"#,          // child_left_rec
            r#"e_4 -> Num"#,                                              //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (608, vec![
            // e -> <R> e "!" e | e "*" e | e "+" e | Num
            r#"e -> e_6 e_1"#,                                            // parent_left_rec | parent_amb
            r#"e_1 -> <R> "!" e_4 e_1 | "*" e_4 e_1 | "+" e_2 e_1 | ε"#,  // child_left_rec
            r#"e_2 -> e_6 e_3"#,                                          // parent_left_rec
            r#"e_3 -> <R,G> "!" e_4 e_3 | <G> "*" e_4 e_3 | ε"#,          // child_left_rec
            r#"e_4 -> e_6 e_5"#,                                          // parent_left_rec
            r#"e_5 -> <R,G> "!" e_4 e_5 | ε"#,                            // child_left_rec
            r#"e_6 -> Num"#,                                              //
        ], vec![1536, 4, 512, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0), Some(4), Some(0)]),
        // ----- postfix op
        (609, vec![
            // e -> e "*" e | e "+" e | e "!" | Num
            r#"e -> e_4 e_1"#,                                    // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "+" e_2 e_1 | "!" e_1 | ε"#,  // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                  // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                      // child_left_rec
            r#"e_4 -> Num"#,                                      //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (610, vec![
            // e -> e "*" e | e "!" | e "+" e | Num
            r#"e -> e_4 e_1"#,                                    // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "!" e_1 | "+" e_2 e_1 | ε"#,  // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                  // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | <G> "!" e_3 | ε"#,        // child_left_rec
            r#"e_4 -> Num"#,                                      //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (611, vec![
            // e -> e "!" | e "*" e | e "+" e | Num
            r#"e -> e_6 e_1"#,                                    // parent_left_rec | parent_amb
            r#"e_1 -> "!" e_1 | "*" e_4 e_1 | "+" e_2 e_1 | ε"#,  // child_left_rec
            r#"e_2 -> e_6 e_3"#,                                  // parent_left_rec
            r#"e_3 -> <G> "!" e_3 | <G> "*" e_4 e_3 | ε"#,        // child_left_rec
            r#"e_4 -> e_6 e_5"#,                                  // parent_left_rec
            r#"e_5 -> <G> "!" e_5 | ε"#,                          // child_left_rec
            r#"e_6 -> Num"#,                                      //
        ], vec![1536, 4, 512, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0), Some(4), Some(0)]),
        // ----- same priority
        (612, vec![
            // e -> e "!" e | e "*" e | e "+" e | Num
            r#"e -> e_6 e_1"#,                                        // parent_left_rec | parent_amb
            r#"e_1 -> "!" e_6 e_1 | "*" e_4 e_1 | "+" e_2 e_1 | ε"#,  // child_left_rec
            r#"e_2 -> e_6 e_3"#,                                      // parent_left_rec
            r#"e_3 -> <G> "!" e_6 e_3 | <G> "*" e_4 e_3 | ε"#,        // child_left_rec
            r#"e_4 -> e_6 e_5"#,                                      // parent_left_rec
            r#"e_5 -> <G> "!" e_6 e_5 | ε"#,                          // child_left_rec
            r#"e_6 -> Num"#,                                          //
        ], vec![1536, 4, 512, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0), Some(4), Some(0)]),
        (613, vec![
            // e -> e "*" e | e "+" e | <P> e "!" e | Num
            r#"e -> e_4 e_1"#,                                        // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "+" e_2 e_1 | "!" e_2 e_1 | ε"#,  // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                      // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                          // child_left_rec
            r#"e_4 -> Num"#,                                          //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (614, vec![
            // e -> e "*" e | <P> e "!" e | e "+" e | Num
            r#"e -> e_4 e_1"#,                                        // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "!" e_4 e_1 | "+" e_2 e_1 | ε"#,  // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                      // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | <G> "!" e_4 e_3 | ε"#,        // child_left_rec
            r#"e_4 -> Num"#,                                          //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        // ----- postfix & prefix ops:
        (630, vec![
            // e -> e "*" e | e "+" | "!" e | Num
            r#"e -> e_2 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> <G> "*" e_2 e_1 | <G> "+" e_1 | ε"#,      // child_left_rec
            r#"e_2 -> "!" e | Num"#,                            // right_rec
        ], vec![1536, 4, 2], vec![None, Some(0), Some(0)]),
        (631, vec![
            // e -> e "*" e | e "+" | <R> "!" e | Num
            r#"e -> e_2 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> <G> "*" e_2 e_1 | <G> "+" e_1 | ε"#,      // child_left_rec
            r#"e_2 -> <R> "!" e | Num"#,                        // right_rec
        ], vec![1536, 4, 2], vec![None, Some(0), Some(0)]),
        (632, vec![
            // e -> e "*" e | <R> e "+" | "!" e | Num
            r#"e -> e_2 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> <G> "*" e_2 e_1 | <R,G> "+" e_1 | ε"#,    // child_left_rec
            r#"e_2 -> "!" e | Num"#,                            // right_rec
        ], vec![1536, 4, 2], vec![None, Some(0), Some(0)]),

        /* template:
        (1, vec![
        ], vec![], vec![]),
        */
    ];
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;

    test_prs_transforms(
        tests,
        |mut prs| {
            prs.remove_recursion();
            prs
        },
        VERBOSE, SHOW_ANSWER_ONLY, true);
}

#[test]
fn prs_left_factorize() {
    let tests = vec![
        (700, vec![
            // a -> A | A B
            r#"a -> A a_1"#,                                    // parent_left_fact
            r#"a_1 -> B | ε"#,                                  // child_left_fact
        ], vec![32, 64], vec![None, Some(0)]),
        (701, vec![
            // a -> A | A B | C
            r#"a -> A a_1 | C"#,                                // parent_left_fact
            r#"a_1 -> B | ε"#,                                  // child_left_fact
        ], vec![32, 64], vec![None, Some(0)]),
        (702, vec![
            // a -> A B | A C
            r#"a -> A a_1"#,                                    // parent_left_fact
            r#"a_1 -> B | C"#,                                  // child_left_fact
        ], vec![32, 64], vec![None, Some(0)]),
        (703, vec![
            // a -> B | A B | A C
            r#"a -> B | A a_1"#,                                // parent_left_fact
            r#"a_1 -> B | C"#,                                  // child_left_fact
        ], vec![32, 64], vec![None, Some(0)]),
        (704, vec![
            // a -> A B C | B B C | B C | B B A
            r#"a -> A B C | B a_1"#,                            // parent_left_fact
            r#"a_1 -> B a_2 | C"#,                              // parent_left_fact | child_left_fact
            r#"a_2 -> A | C"#,                                  // child_left_fact
        ], vec![32, 96, 64], vec![None, Some(0), Some(1)]),
        /* template:
        (1, vec![
        ], vec![], vec![]),
        */
    ];
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;

    test_prs_transforms(
        tests,
        |mut prs| {
            prs.left_factorize();
            prs
        },
        VERBOSE, SHOW_ANSWER_ONLY, true);
}

#[test]
fn prs_ll1_from() {
    let tests = vec![
        (0, vec![
            // a -> A
            r#"a -> A"#,                                        //
        ], vec![0], vec![None]),
        (100, vec![
            // a -> A*
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (101, vec![
            // a -> A+
            r#"a -> a_1"#,                                      // parent_+_or_* | plus
            r#"a_1 -> A a_2"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6144, 4129, 64], vec![None, Some(0), Some(1)]),
        (106, vec![
            // a -> (A (b ",")* ";")* C
            // b -> B
            r#"a -> a_2 C"#,                                    // parent_+_or_*
            r#"b -> B"#,                                        //
            r#"a_1 -> b "," a_1 | ε"#,                          // child_+_or_*
            r#"a_2 -> A a_1 ";" a_2 | ε"#,                      // child_+_or_* | parent_+_or_*
        ], vec![2048, 0, 1, 2049], vec![None, None, Some(3), Some(0)]),
        (107, vec![
            // a -> (A (b ",")+ ";")+ C
            // b -> B
            r#"a -> a_2 C"#,                                    // parent_+_or_* | plus
            r#"b -> B"#,                                        //
            r#"a_1 -> b "," a_3"#,                              // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> A a_1 ";" a_4"#,                          // child_+_or_* | parent_left_fact | parent_+_or_* | plus
            r#"a_3 -> a_1 | ε"#,                                // child_left_fact
            r#"a_4 -> a_2 | ε"#,                                // child_left_fact
        ], vec![6144, 0, 4129, 6177, 64, 64], vec![None, None, Some(3), Some(0), Some(2), Some(3)]),
        (200, vec![
            // a -> A (<L=i> B)* C
            r#"a -> A i C"#,                                    // parent_+_or_*
            r#"i -> B i | ε"#,                                  // child_+_or_* | L-form
        ], vec![2048, 129], vec![None, Some(0)]),
        (201, vec![
            // a -> A (<L=i> B)+ C
            r#"a -> A i C"#,                                    // parent_+_or_* | plus
            r#"i -> B a_1"#,                                    // child_+_or_* | parent_left_fact | L-form | plus
            r#"a_1 -> i | ε"#,                                  // child_left_fact
        ], vec![6144, 4257, 64], vec![None, Some(0), Some(1)]),
        (208, vec![
            // a -> (<L=i> A (<L=j> b ",")* ";")* C
            // b -> B
            r#"a -> i C"#,                                      // parent_+_or_*
            r#"i -> A j ";" i | ε"#,                            // child_+_or_* | L-form | parent_+_or_*
            r#"j -> b "," j | ε"#,                              // child_+_or_* | L-form
            r#"b -> B"#,                                        //
        ], vec![2048, 2177, 129, 0], vec![None, Some(0), Some(1), None]),
        (209, vec![
            // a -> (<L=i> A (<L=j> b ",")+ ";")+ C
            // b -> B
            r#"a -> i C"#,                                      // parent_+_or_* | plus
            r#"i -> A j ";" a_1"#,                              // child_+_or_* | parent_left_fact | L-form | parent_+_or_* | plus
            r#"j -> b "," a_2"#,                                // child_+_or_* | parent_left_fact | L-form | plus
            r#"b -> B"#,                                        //
            r#"a_1 -> i | ε"#,                                  // child_left_fact
            r#"a_2 -> j | ε"#,                                  // child_left_fact
        ], vec![6144, 6305, 4257, 0, 64, 64], vec![None, Some(0), Some(1), None, Some(1), Some(2)]),
        (300, vec![
            // a -> "?" a | "!"
            r#"a -> "?" a | "!""#,                              // right_rec
        ], vec![2], vec![None]),
        (500, vec![
            // a -> a "!" | "?"
            r#"a -> "?" a_1"#,                                  // parent_left_rec
            r#"a_1 -> "!" a_1 | ε"#,                            // child_left_rec
        ], vec![512, 4], vec![None, Some(0)]),
        (601, vec![
            // e -> e "*" e | e "+" e | Num | Id
            r#"e -> e_4 e_1"#,                                  // parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "+" e_2 e_1 | ε"#,          // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                    // child_left_rec
            r#"e_4 -> Num | Id"#,                               //
        ], vec![1536, 4, 512, 4, 0], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (634, vec![
            // e -> e "+" | <L=e> "!" e | Num
            r#"e -> "!" e | Num e_1"#,                          // right_rec | L-form | parent_left_rec
            r#"e_1 -> "+" e_1 | ε"#,                            // child_left_rec
        ], vec![642, 4], vec![None, Some(0)]),
        (800, vec![
            // a -> A?*
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (801, vec![
            // a -> A?+
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (802, vec![
            // a -> A*?
            r#"a -> a_1 | ε"#,                                  // parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (803, vec![
            // a -> A+?
            r#"a -> a_1 | ε"#,                                  // parent_+_or_* | plus
            r#"a_1 -> A a_2"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6144, 4129, 64], vec![None, Some(0), Some(1)]),
        (810, vec![
            // a -> A* a
            r#"a -> a_1 a"#,                                    // right_rec | parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
        ], vec![2050, 1], vec![None, Some(0)]),
        (811, vec![
            // a -> A+ a
            r#"a -> a_1 a"#,                                    // right_rec | parent_+_or_* | plus
            r#"a_1 -> A a_2"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6146, 4129, 64], vec![None, Some(0), Some(1)]),
        (812, vec![
            // a -> (A a)*
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a a_1 | ε"#,                            // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (813, vec![
            // a -> (A a)+
            r#"a -> a_1"#,                                      // parent_+_or_* | plus
            r#"a_1 -> A a a_2"#,                                // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6144, 4129, 64], vec![None, Some(0), Some(1)]),
        (820, vec![
            // a -> a A* | B
            r#"a -> B a_2"#,                                    // parent_left_rec | parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
            r#"a_2 -> a_1 a_2 | ε"#,                            // child_left_rec
        ], vec![2560, 1, 4], vec![None, Some(0), Some(0)]),
        (821, vec![
            // a -> a A+ | B
            r#"a -> B a_2"#,                                    // parent_left_rec | parent_+_or_* | plus
            r#"a_1 -> A a_3"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> a_1 a_2 | ε"#,                            // child_left_rec
            r#"a_3 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6656, 4129, 4, 64], vec![None, Some(0), Some(0), Some(1)]),
        (822, vec![
            // a -> (a A)* | B
            r#"a -> a_1 | B"#,                                  // parent_+_or_*
            r#"a_1 -> a A a_1 | ε"#,                            // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (823, vec![
            // a -> (a A)+ | B
            r#"a -> a_1 | B"#,                                  // parent_+_or_* | plus
            r#"a_1 -> a A a_2"#,                                // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6144, 4129, 64], vec![None, Some(0), Some(1)]),
        (830, vec![
            // a -> (a A)* a | B
            r#"a -> a_1 a | B"#,                                // right_rec | parent_+_or_*
            r#"a_1 -> a A a_1 | ε"#,                            // child_+_or_*
        ], vec![2050, 1], vec![None, Some(0)]),
        (831, vec![
            // a -> (a A)+ a | B
            r#"a -> a_1 a | B"#,                                // right_rec | parent_+_or_* | plus
            r#"a_1 -> a A a_2"#,                                // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6146, 4129, 64], vec![None, Some(0), Some(1)]),
        (832, vec![
            // a -> a (A a)* | B
            r#"a -> B a_2"#,                                    // parent_left_rec | parent_+_or_*
            r#"a_1 -> A a a_1 | ε"#,                            // child_+_or_*
            r#"a_2 -> a_1 a_2 | ε"#,                            // child_left_rec
        ], vec![2560, 1, 4], vec![None, Some(0), Some(0)]),
        (833, vec![
            // a -> a (A a)+ | B
            r#"a -> B a_2"#,                                    // parent_left_rec | parent_+_or_* | plus
            r#"a_1 -> A a a_3"#,                                // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> a_1 a_2 | ε"#,                            // child_left_rec
            r#"a_3 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6656, 4129, 4, 64], vec![None, Some(0), Some(0), Some(1)]),
        (834, vec![
            // a -> (a A a)* | B
            r#"a -> a_1 | B"#,                                  // parent_+_or_*
            r#"a_1 -> a A a a_1 | ε"#,                          // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (840, vec![
            // a -> (A B | A C)*
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_2 | ε"#,                              // child_+_or_* | parent_left_fact
            r#"a_2 -> B a_1 | C a_1"#,                          // child_left_fact
        ], vec![2048, 33, 64], vec![None, Some(0), Some(1)]),
        (841, vec![
            // a -> (A B | A C)+
            r#"a -> a_1"#,                                      // parent_+_or_* | plus
            r#"a_1 -> A a_2"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> B a_3 | C a_4"#,                          // parent_left_fact | child_left_fact
            r#"a_3 -> a_1 | ε"#,                                // child_left_fact
            r#"a_4 -> a_1 | ε"#,                                // child_left_fact
        ], vec![6144, 4129, 96, 64, 64], vec![None, Some(0), Some(1), Some(2), Some(2)]),

        // 842 and 843 are not left-factorized because the common items (A) are hidden below the *+
        (842, vec![
            // a -> A* B* | A* C*
            r#"a -> a_1 a_2 | a_3 a_4"#,                        // parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
            r#"a_2 -> B a_2 | ε"#,                              // child_+_or_*
            r#"a_3 -> A a_3 | ε"#,                              // child_+_or_*
            r#"a_4 -> C a_4 | ε"#,                              // child_+_or_*
        ], vec![2048, 1, 1, 1, 1], vec![None, Some(0), Some(0), Some(0), Some(0)]),
        (843, vec![
            // a -> A+ B+ | A+ C+
            r#"a -> a_1 a_2 | a_3 a_4"#,                        // parent_+_or_* | plus
            r#"a_1 -> A a_5"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> B a_6"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_3 -> A a_7"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_4 -> C a_8"#,                                  // child_+_or_* | parent_left_fact | plus
            r#"a_5 -> a_1 | ε"#,                                // child_left_fact
            r#"a_6 -> a_2 | ε"#,                                // child_left_fact
            r#"a_7 -> a_3 | ε"#,                                // child_left_fact
            r#"a_8 -> a_4 | ε"#,                                // child_left_fact
        ], vec![6144, 4129, 4129, 4129, 4129, 64, 64, 64, 64], vec![None, Some(0), Some(0), Some(0), Some(0), Some(1), Some(2), Some(3), Some(4)]),
        (850, vec![
            // a -> a A a a | B
            r#"a -> a_2 a_1"#,                                  // parent_left_rec | parent_amb
            r#"a_1 -> A a a_2 a_1 | ε"#,                        // child_left_rec
            r#"a_2 -> B"#,                                      //
        ], vec![1536, 4, 0], vec![None, Some(0), Some(0)]),
        (860, vec![
            // a -> A B a | A C a | D
            r#"a -> A a_1 | D"#,                                // right_rec | parent_left_fact
            r#"a_1 -> B a | C a"#,                              // child_left_fact
        ], vec![34, 64], vec![None, Some(0)]),
        (861, vec![
            // a -> A (B | C) a <L=a> | D
            r#"a -> A a_1 | D"#,                                // right_rec | parent_left_fact | L-form
            r#"a_1 -> B a | C a"#,                              // child_left_fact
        ], vec![162, 64], vec![None, Some(0)]),
        (870, vec![
            // a -> a A B | a A C | D
            r#"a -> D a_1"#,                                    // parent_left_rec
            r#"a_1 -> A a_2 | ε"#,                              // child_left_rec | parent_left_fact
            r#"a_2 -> B a_1 | C a_1"#,                          // child_left_fact
        ], vec![512, 36, 64], vec![None, Some(0), Some(1)]),
        (901, vec![
            // file -> header? file_item*
            // file_item -> option | declaration | rule
            // header -> "lexicon" Id ";"
            // declaration -> "mode" Id ";"
            // option -> "channels" "{" Id ("," Id)* "}"
            // rule -> "fragment" Id ":" match ";" | Id ":" match ("->" actions)? ";"
            // actions -> action ("," action)*
            // action -> "mode" "(" Id ")" | "push" "(" Id ")" | "pop" | "skip" | "more" | "type" "(" Id ")" | "channel" "(" Id ")"
            // match -> alt_items
            // alt_items -> alt_items "|" alt_item | alt_item
            // alt_item -> repeat_item+
            // repeat_item -> item "*" "?"? | item "+" "?"? | item "?"?
            // item -> Id | CharLit (".." CharLit)? | StrLit | char_set | "(" alt_items ")" | "~" item
            // char_set -> "[" char_set_one+ "]" | "." | FixedSet
            // char_set_one -> SetChar "-" SetChar | SetChar | FixedSet
            r#"file -> header file_1 | file_1"#,                                                                                        // parent_+_or_*
            r#"file_item -> option | declaration | rule"#,                                                                              //
            r#"header -> "lexicon" Id ";""#,                                                                                            //
            r#"declaration -> "mode" Id ";""#,                                                                                          //
            r#"option -> "channels" "{" Id option_1 "}""#,                                                                              // parent_+_or_*
            r#"rule -> "fragment" Id ":" match ";" | Id ":" match rule_1"#,                                                             // parent_left_fact
            r#"actions -> action actions_1"#,                                                                                           // parent_+_or_*
            r#"action -> "mode" "(" Id ")" | "push" "(" Id ")" | "pop" | "skip" | "more" | "type" "(" Id ")" | "channel" "(" Id ")""#,  //
            r#"match -> alt_items"#,                                                                                                    //
            r#"alt_items -> alt_item alt_items_1"#,                                                                                     // parent_left_rec
            r#"alt_item -> alt_item_1"#,                                                                                                // parent_+_or_* | plus
            r#"repeat_item -> item repeat_item_1"#,                                                                                     // parent_left_fact
            r#"item -> "(" alt_items ")" | "~" item | Id | CharLit item_1 | StrLit | char_set"#,                                        // right_rec | parent_left_fact
            r#"char_set -> "[" char_set_1 "]" | "." | FixedSet"#,                                                                       // parent_+_or_* | plus
            r#"char_set_one -> FixedSet | SetChar char_set_one_1"#,                                                                     // parent_left_fact
            r#"file_1 -> file_item file_1 | ε"#,                                                                                        // child_+_or_*
            r#"option_1 -> "," Id option_1 | ε"#,                                                                                       // child_+_or_*
            r#"actions_1 -> "," action actions_1 | ε"#,                                                                                 // child_+_or_*
            r#"alt_item_1 -> repeat_item alt_item_2"#,                                                                                  // child_+_or_* | parent_left_fact | plus
            r#"char_set_1 -> char_set_one char_set_2"#,                                                                                 // child_+_or_* | parent_left_fact | plus
            r#"alt_items_1 -> "|" alt_item alt_items_1 | ε"#,                                                                           // child_left_rec
            r#"rule_1 -> "->" actions ";" | ";""#,                                                                                      // child_left_fact
            r#"repeat_item_1 -> "+" repeat_item_2 | "?" | "*" repeat_item_3 | ε"#,                                                      // parent_left_fact | child_left_fact
            r#"item_1 -> ".." CharLit | ε"#,                                                                                            // child_left_fact
            r#"char_set_one_1 -> "-" SetChar | ε"#,                                                                                     // child_left_fact
            r#"alt_item_2 -> alt_item_1 | ε"#,                                                                                          // child_left_fact
            r#"char_set_2 -> char_set_1 | ε"#,                                                                                          // child_left_fact
            r#"repeat_item_2 -> "?" | ε"#,                                                                                              // child_left_fact
            r#"repeat_item_3 -> "?" | ε"#,                                                                                              // child_left_fact
        ], vec![
            2048, 0, 0, 0, 2048, 32, 2048, 0, 0, 512, 6144, 32, 34, 6144, 32, 1, 1, 1, 4129, 4129, 4, 64, 96, 64, 64, 64, 64, 64, 64
        ], vec![
            None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, Some(0), Some(4), Some(6),
            Some(10), Some(13), Some(9), Some(5), Some(11), Some(12), Some(14), Some(18), Some(19), Some(22), Some(22)
        ]),
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
            ProdRuleSet::<LL1>::build_from(prs)
        },
        VERBOSE, SHOW_ANSWER_ONLY, true);
}

#[test]
#[should_panic]
/// We test that the code compiles, but it must also panic because the `remove_ambiguity()`
/// method isn't implemented yet for LR grammars.
fn prs_lr_from() {
    let mut test_id = 0;
    loop {
        let rules = old_build_rts_prs::build_prs(test_id, false);
        if rules.prules.is_empty() {
            break;
        }
        let _lr = ProdRuleSet::<LR>::build_from(rules);
        test_id += 1;
    }
}

fn test_first_or_follow<F>(tests: Vec<(u32, VarId, HashMap<&str, &str>)>, mut f: F, verbose: bool, show_answer_only: bool, show_rules: bool)
where
    F: FnMut(&mut ProdRuleSet<LL1>) -> HashMap<Symbol, HashSet<Symbol>>
{
    let mut errors = 0;
    for (test_id, start, expected) in tests {
        if verbose && !show_answer_only {
            println!("{:=<80}\ntest {test_id}:", "");
        }
        let mut ll1 = TestRules(test_id).to_prs_ll1().unwrap();
        ll1.set_start(start);
        let set_to_test = f(&mut ll1);
        let symtab = ll1.get_symbol_table();
        let result = set_to_test.iter()
            .map(|(s, hs)| {
                let mut values = hs.into_iter().to_vec();
                values.sort();
                (s.to_str_name(symtab), values.into_iter().map(|s2| s2.to_str_name(symtab)).join(", "))
            })
            .collect::<HashMap<_, _>>();
        if verbose || show_answer_only {
            println!("        ({test_id}, {start}, hashmap![");
            if !show_answer_only || show_rules {
                ll1.print_rules(true, false);
            }
            let mut syms = set_to_test.keys().to_vec();
            syms.sort();
            for s in syms {
                let str = s.to_str_name(symtab);
                println!("            \"{str}\" => \"{}\",", result.get(&str).unwrap());
            }
            println!("        ]),");
        }
        let fail1 = result != expected.into_iter().map(|(s1, s2)| (s1.to_string(), s2.to_string())).collect();
        let fail2 = !ll1.log.has_no_errors();
        let fail3 = !ll1.log.has_no_warnings();
        if fail1 || fail2 || fail3 {
            errors += 1;
            if !show_answer_only {
                print!("## ERROR ## test {test_id} failed");
                if fail1 { print!(", wrong result"); }
                if fail2 { print!(", errors in log"); }
                if fail3 { print!(", warnings in log"); }
                println!();
                if fail2 || fail3 {
                    println!("Log:\n{}", ll1.log);
                }
            }
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}

#[test]
fn prs_calc_first() {
    let tests: Vec<(u32, VarId, HashMap<&str, &str>)> = vec![
        (0, 0, hashmap![
            "A" => "A",
            "a" => "A",
        ]),
        (1, 0, hashmap![
            "A" => "A",
            "B" => "B",
            "a" => "A",
        ]),
        (2, 0, hashmap![
            "A" => "A",
            "B" => "B",
            "a" => "A, B",
        ]),
        (500, 0, hashmap![
            "Not" => "Not",
            "Question" => "Question",
            "a" => "Question",
            "a_1" => "Not, ε",
            "ε" => "ε",
        ]),
        (501, 0, hashmap![
            "B" => "B",
            "C" => "C",
            "A" => "A",
            "a" => "A",
            "a_1" => "B, C, ε",
            "ε" => "ε",
        ]),
        (600, 0, hashmap![
            "Add" => "Add",
            "Num" => "Num",
            "e" => "Num",
            "e_1" => "Add, ε",
            "e_2" => "Num",
            "ε" => "ε",
        ]),
        (603, 0, hashmap![
            "Mul" => "Mul",
            "Add" => "Add",
            "Op" => "Op",
            "Num" => "Num",
            "e" => "Op, Num",
            "e_1" => "Mul, Add, ε",
            "e_2" => "Op, Num",
            "e_3" => "Mul, ε",
            "e_4" => "Op, Num",
            "ε" => "ε",
        ]),
        (860, 0, hashmap![
            "A" => "A",
            "B" => "B",
            "C" => "C",
            "D" => "D",
            "a" => "A, D",
            "a_1" => "B, C",
        ]),
        /* template:
        (, 0, hashmap![]),
        */
    ];
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;
    const SHOW_RULES: bool = false;
    test_first_or_follow(tests, |ll1| ll1.calc_first(), VERBOSE, SHOW_ANSWER_ONLY, SHOW_RULES);
}

#[test]
fn prs_calc_follow() {
    let tests: Vec<(u32, VarId, HashMap<&str, &str>)> = vec![
        (0, 0, hashmap![
            "a" => "$",
        ]),
        (100, 0, hashmap![
            "a" => "$",
            "a_1" => "$",
        ]),
        (300, 0, hashmap![
            "a" => "$",
        ]),
        (500, 0, hashmap![
            "a" => "$",
            "a_1" => "$",
        ]),
        (600, 0, hashmap![
            "e" => "$",
            "e_1" => "$",
            "e_2" => "Add, $",
        ]),
        (603, 0, hashmap![
            "e" => "Mul, Add, $",
            "e_1" => "Mul, Add, $",
            "e_2" => "Mul, Add, $",
            "e_3" => "Mul, Add, $",
            "e_4" => "Mul, Add, $",
        ]),
        (860, 0, hashmap![
            "a" => "$",
            "a_1" => "$",
        ]),
        /* template:
        (, 0, hashmap![]),
        */
    ];
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;
    const SHOW_RULES: bool = false;
    test_first_or_follow(tests, |ll1| {
        let first = ll1.calc_first();
        ll1.calc_follow(&first)
    }, VERBOSE, SHOW_ANSWER_ONLY, SHOW_RULES);
}

#[test]
fn prs_calc_table() {
    let tests = vec![
        (0, 0, false, vec![
            //   |  A   $
            // --+---------
            // a |  0   p
              0,   2,
        ]),
        (100, 0, false, vec![
            // - 0: a -> a_1
            // - 1: a_1 -> A a_1
            // - 2: a_1 -> ε
            //
            //     |  A   $
            // ----+---------
            // a   |  0   0
            // a_1 |  1   2
              0,   0,
              1,   2,
        ]),
        (300, 0, false, vec![
            // - 0: a -> "?" a
            // - 1: a -> "!"
            //
            //   |  ?   !   $
            // --+-------------
            // a |  0   1   p
              0,   1,   3,
        ]),
        (500, 0, false, vec![
            // - 0: a -> "?" a_1
            // - 1: a_1 -> "!" a_1
            // - 2: a_1 -> ε
            //
            //     |  !   ?   $
            // ----+-------------
            // a   |  .   0   p
            // a_1 |  1   .   2
              3,   0,   4,
              1,   3,   2,
        ]),
        (600, 0, false, vec![
            // - 0: e -> e_2 e_1
            // - 1: e_1 -> "+" e_2 e_1
            // - 2: e_1 -> ε
            // - 3: e_2 -> Num
            //
            //     |  +  Num  $
            // ----+-------------
            // e   |  .   0   p
            // e_1 |  1   .   2
            // e_2 |  p   3   p
              4,   0,   5,
              1,   4,   2,
              5,   3,   5,
        ]),
        (603, 0, false, vec![
            // - 0: e -> e_4 e_1
            // - 1: e_1 -> "*" e_4 e_1
            // - 2: e_1 -> "+" e_2 e_1
            // - 3: e_1 -> ε
            // - 4: e_2 -> e_4 e_3
            // - 5: e_3 -> "*" e_4 e_3
            // - 6: e_3 -> ε
            // - 7: e_4 -> "!" e
            // - 8: e_4 -> Num
            //
            //     |  *   +   !  Num  $
            // ----+---------------------
            // e   |  p   p   0   0   p
            // e_1 |  1   2   .   .   3
            // e_2 |  p   p   4   4   p
            // e_3 |  5   6   .   .   6
            // e_4 |  p   p   7   8   p
             10,  10,   0,   0,  10,
              1,   2,   9,   9,   3,
             10,  10,   4,   4,  10,
              5,   6,   9,   9,   6,
             10,  10,   7,   8,  10,
        ]),
        (860, 0, false, vec![
            // - 0: a -> A a_1
            // - 1: a -> D
            // - 2: a_1 -> B a
            // - 3: a_1 -> C a
            //
            //     |  A   B   C   D   $
            // ----+---------------------
            // a   |  0   .   .   1   p
            // a_1 |  .   2   3   .   p
              0,   4,   4,   1,   5,
              4,   2,   3,   4,   5,
        ]),
        (900, 0, false, vec![
            // - 0: ruleset -> rule_iter
            // - 1: rule_iter -> rule ruleset_1
            // - 2: rule -> rule_nt rule_1
            // - 3: rule_nt -> NT
            // - 4: rts_expr -> "&" rts_children
            // - 5: rts_expr -> "|" rts_children
            // - 6: rts_expr -> "+" rts_children
            // - 7: rts_expr -> "*" rts_children
            // - 8: rts_expr -> "?" rts_children
            // - 9: rts_expr -> item
            // - 10: rts_children -> "(" rts_children_1 ")"
            // - 11: prs_expr -> prs_expr_6 prs_expr_1
            // - 12: item -> NT
            // - 13: item -> T
            // - 14: item -> "ε"
            // - 15: item -> "<L>"
            // - 16: item -> "<P>"
            // - 17: item -> "<R>"
            // - 18: rts_children_1 -> rts_expr rts_children_1
            // - 19: rts_children_1 -> ε
            // - 20: prs_expr_1 -> "+" prs_expr_1
            // - 21: prs_expr_1 -> "*" prs_expr_1
            // - 22: prs_expr_1 -> "?" prs_expr_1
            // - 23: prs_expr_1 -> prs_expr_4 prs_expr_1
            // - 24: prs_expr_1 -> "|" prs_expr_2 prs_expr_1
            // - 25: prs_expr_1 -> ε
            // - 26: prs_expr_2 -> prs_expr_6 prs_expr_3
            // - 27: prs_expr_3 -> "+" prs_expr_3
            // - 28: prs_expr_3 -> "*" prs_expr_3
            // - 29: prs_expr_3 -> "?" prs_expr_3
            // - 30: prs_expr_3 -> prs_expr_4 prs_expr_3
            // - 31: prs_expr_3 -> ε
            // - 32: prs_expr_4 -> prs_expr_6 prs_expr_5
            // - 33: prs_expr_5 -> "+" prs_expr_5
            // - 34: prs_expr_5 -> "*" prs_expr_5
            // - 35: prs_expr_5 -> "?" prs_expr_5
            // - 36: prs_expr_5 -> ε
            // - 37: prs_expr_6 -> "(" prs_expr ")"
            // - 38: prs_expr_6 -> item
            // - 39: ruleset_1 -> rule_iter
            // - 40: ruleset_1 -> ε
            // - 41: rule_1 -> "=>" rts_expr ";"
            // - 42: rule_1 -> "->" prs_expr ";"
            //
            //                | =>   ;  ->  NT   &   |   +   *   ?   (   )   T   ε  <L> <P> <R>  $
            // ---------------+---------------------------------------------------------------------
            // ruleset        |  .   .   .   0   .   .   .   .   .   .   .   .   .   .   .   .   p
            // rule_iter      |  .   .   .   1   .   .   .   .   .   .   .   .   .   .   .   .   p
            // rule           |  .   .   .   2   .   .   .   .   .   .   .   .   .   .   .   .   p
            // rule_nt        |  p   .   p   3   .   .   .   .   .   .   .   .   .   .   .   .   .
            // rts_expr       |  .   p   .   9   4   5   6   7   8   .   p   9   9   9   9   9   .
            // rts_children   |  .   p   .   p   p   p   p   p   p  10   p   p   p   p   p   p   .
            // prs_expr       |  .   p   .  11   .   .   .   .   .  11   p  11  11  11  11  11   .
            // item           |  .   p   .  12   p   p   p   p   p   p   p  13  14  15  16  17   .
            // rts_children_1 |  .   .   .  18  18  18  18  18  18   .  19  18  18  18  18  18   .
            // prs_expr_1     |  .  25   .  23   .  24  20  21  22  23  25  23  23  23  23  23   .
            // prs_expr_2     |  .   p   .  26   .   p   p   p   p  26   p  26  26  26  26  26   .
            // prs_expr_3     |  .  31   .  30   .  31  27  28  29  30  31  30  30  30  30  30   .
            // prs_expr_4     |  .   p   .  32   .   p   p   p   p  32   p  32  32  32  32  32   .
            // prs_expr_5     |  .  36   .  36   .  36  33  34  35  36  36  36  36  36  36  36   .
            // prs_expr_6     |  .   p   .  38   .   p   p   p   p  37   p  38  38  38  38  38   .
            // ruleset_1      |  .   .   .  39   .   .   .   .   .   .   .   .   .   .   .   .  40
            // rule_1         | 41   .  42   p   .   .   .   .   .   .   .   .   .   .   .   .   p
             43,  43,  43,   0,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  44,
             43,  43,  43,   1,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  44,
             43,  43,  43,   2,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  44,
             44,  43,  44,   3,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,
             43,  44,  43,   9,   4,   5,   6,   7,   8,  43,  44,   9,   9,   9,   9,   9,  43,
             43,  44,  43,  44,  44,  44,  44,  44,  44,  10,  44,  44,  44,  44,  44,  44,  43,
             43,  44,  43,  11,  43,  43,  43,  43,  43,  11,  44,  11,  11,  11,  11,  11,  43,
             43,  44,  43,  12,  44,  44,  44,  44,  44,  44,  44,  13,  14,  15,  16,  17,  43,
             43,  43,  43,  18,  18,  18,  18,  18,  18,  43,  19,  18,  18,  18,  18,  18,  43,
             43,  25,  43,  23,  43,  24,  20,  21,  22,  23,  25,  23,  23,  23,  23,  23,  43,
             43,  44,  43,  26,  43,  44,  44,  44,  44,  26,  44,  26,  26,  26,  26,  26,  43,
             43,  31,  43,  30,  43,  31,  27,  28,  29,  30,  31,  30,  30,  30,  30,  30,  43,
             43,  44,  43,  32,  43,  44,  44,  44,  44,  32,  44,  32,  32,  32,  32,  32,  43,
             43,  36,  43,  36,  43,  36,  33,  34,  35,  36,  36,  36,  36,  36,  36,  36,  43,
             43,  44,  43,  38,  43,  44,  44,  44,  44,  37,  44,  38,  38,  38,  38,  38,  43,
             43,  43,  43,  39,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  40,
             41,  43,  42,  44,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  43,  44,
        ]),
        (631, 0, false, vec![
            // - 0: e -> e_2 e_1
            // - 1: e_1 -> "*" e_2 e_1
            // - 2: e_1 -> "+" e_1
            // - 3: e_1 -> ε
            // - 4: e_2 -> <R> "!" e     R-assoc (256)
            // - 5: e_2 -> Num
            //
            //     |  *   +   !  Num  $
            // ----+---------------------
            // e   |  p   p   0   0   p
            // e_1 |  1   2   .   .   3
            // e_2 |  p   p   4   5   p
              7,   7,   0,   0,   7,
              1,   2,   6,   6,   3,
              7,   7,   4,   5,   7,
        ]),
        (632, 0, false, vec![
            // - 0: e -> e_2 e_1
            // - 1: e_1 -> "*" e_2 e_1
            // - 2: e_1 -> <R> "+" e_1     R-assoc (256)
            // - 3: e_1 -> ε
            // - 4: e_2 -> "!" e
            // - 5: e_2 -> Num
            //
            //     |  *   +   !  Num  $
            // ----+---------------------
            // e   |  p   p   0   0   p
            // e_1 |  1   2   .   .   3
            // e_2 |  p   p   4   5   p
              7,   7,   0,   0,   7,
              1,   2,   6,   6,   3,
              7,   7,   4,   5,   7,
        ]),
        (812, 0, true, vec![]), // ambiguous grammars
        (813, 0, true, vec![]),
        (820, 0, true, vec![]),
        (821, 0, true, vec![]),
        (822, 0, true, vec![]),
        (823, 0, true, vec![]),
        (830, 0, true, vec![]),
        (831, 0, true, vec![]),
        (832, 0, true, vec![]),
        (833, 0, true, vec![]),
        (834, 0, true, vec![]),
        /* template:
        (, 0, vec![]),
        */
    ];
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;
    const SHOW_RULES: bool = true;
    let mut errors = 0;
    for (test_id, start, expected_is_ambiguous, expected) in tests {
        if VERBOSE && !SHOW_ANSWER_ONLY {
            println!("{:=<80}\ntest {test_id}:", "");
        }
        let msg = format!("## ERROR ## test {test_id}, start={start}");
        let mut ll1 = TestRules(test_id).to_prs_ll1().unwrap();
        ll1.set_start(start);
        let parsing_table = ll1.make_parsing_table(true);
        let LLParsingTable { num_nt, num_t, alts, table, .. } = &parsing_table;
        let result_is_ambiguous = ll1.log.get_warnings().any(|w| w.contains("calc_table: ambiguity"));
        if num_nt * num_t != table.len() {
            if VERBOSE { println!("{msg}: incorrect table size"); }
        }
        if VERBOSE || SHOW_ANSWER_ONLY {
            if SHOW_ANSWER_ONLY && result_is_ambiguous {
                println!("        ({test_id}, {start}, true, vec![]),");
            } else {
                println!("        ({test_id}, {start}, {result_is_ambiguous}, vec![");
                if !SHOW_ANSWER_ONLY || SHOW_RULES {
                    print_alts(&alts, ll1.get_symbol_table());
                    println!("            //");
                }
                parsing_table.print(ll1.get_symbol_table(), 12);
                for i in 0..*num_nt {
                    println!("            {},", (0..*num_t).map(|j| format!("{:3}", table[i * num_t + j])).join(", "));
                }
                println!("        ]),");
            }
        }
        let fail0 = result_is_ambiguous != expected_is_ambiguous;
        let fail1 = !result_is_ambiguous && table != &expected;
        let fail2 = !ll1.log.has_no_errors();
        let fail3 = !result_is_ambiguous && !ll1.log.has_no_warnings();
        if fail0 || fail1 || fail2 || fail3 {
            errors += 1;
            if !SHOW_ANSWER_ONLY {
                print!("## ERROR ## test {test_id} failed");
                if fail0 { print!(", {} ambiguous", if result_is_ambiguous { "is" } else { "isn't" }); }
                if fail1 { print!(", wrong result"); }
                if fail2 { print!(", errors in log"); }
                if fail3 { print!(", warnings in log"); }
                println!();
                if fail2 || fail3 {
                    println!("Log:\n{}", ll1.log);
                }
            }
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}

#[test]
fn prs_grammar_notes() {
    let tests: Vec<(u32, Vec<&str>, Vec<&str>)> = vec![
        (
            1000,
            vec![],
            vec!["recursive rules must have at least one independent alternative"],
        ),
        (
            1001,
            vec![],
            vec!["no terminal in grammar"],
        ),
        (
            1002,
            vec![],
            vec!["no terminal used in the table"],
        ),
        (
            1003,
            vec!["unused nonterminals: NT(1) = b",
                 "unused terminals: T(0) = B"],
            vec![],
        ),
        (
            1004,
            vec![],
            vec!["there are 2 rules but the symbol table has 1 nonterminal symbols: dropping the table"],
        ),
        (
            1005,
            vec![],
            vec!["in a, (<L=x> A <L=y>)*: conflicting <L=x> and <L=y>",
                 "normalize_var(y): error while normalizing the rules, 0 remaining nodes instead of 1"],
        ),
        (
            1006,
            vec![],
            vec!["in a, (<L=x> A | <L=y> B)*: conflicting <L=x> and <L=y>",
                 "normalize_var(y): error while normalizing the rules, 0 remaining nodes instead of 1"],
        ),
        (
            1007,
            vec![],
            vec!["a has an illegal flag L-Form (only used with +, *, or right recursion)",
                 "b has an illegal flag L-Form"],
        ),
        (
            1008,
            vec![],
            vec!["start NT symbol not defined",
                 "no nonterminal in grammar"],
        ),
        (
            1009,
            vec![],
            vec!["e has an illegal flag L-Form"],
        ),
        /* template:
        (
            , 0,
            vec![],
            vec![],
        ),
        */
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, expected_warnings, expected_errors)) in tests.into_iter().enumerate() {
        if VERBOSE {
            println!("{:=<80}\ntest {test_id} with {ll_id:?}:", "");
        }
        let mut ll1 = TestRules(ll_id).to_prs_ll1().unwrap();
        if VERBOSE {
            ll1.print_rules(false, false);
        }
        if ll1.log.num_errors() == 0 {
            let first = ll1.calc_first();
            if ll1.log.num_errors() == 0 {
                let follow = ll1.calc_follow(&first);
                if ll1.log.num_errors() == 0 {
                    _ = Some(ll1.calc_table(&first, &follow, false));
                }
            }
            if VERBOSE {
                println!("=>");
                ll1.print_rules(false, false);
            }
        }
        if VERBOSE {
            println!("Log:\n{}", ll1.log);
        }
        let msg = format!("test {test_id}/{ll_id:?}: ");
        assert_eq!(ll1.log.num_errors(), expected_errors.len(), "{msg}failed on # errors");
        assert_eq!(ll1.log.num_warnings(), expected_warnings.len(), "{msg}failed on # warnings");
        let err_discr = ll1.log.get_errors().zip(expected_errors).filter_map(|(e, ee)|
            if !e.contains(ee) { Some(format!("- \"{e}\" doesn't contain \"{ee}\"")) } else { None }
        ).to_vec();
        assert!(err_discr.is_empty(), "{msg}has discrepancies in the expected error messages:\n{}", err_discr.join("\n"));
        let warn_discr = ll1.log.get_warnings().zip(expected_warnings).filter_map(|(w, ew)|
            if !w.contains(ew) { Some(format!("- \"{w}\" doesn't contain \"{ew}\"")) } else { None }
        ).to_vec();
        assert!(warn_discr.is_empty(), "{msg}has discrepancies in the expected warning messages:\n{}", warn_discr.join("\n"));
   }
}

#[test]
fn build_prs_error() {
    let rts = TestRules(1004).to_rts_general().unwrap();
    let text = format!("rts errors: {}", rts.get_log().num_errors());
    assert_eq!(rts.get_log().num_errors(), 0, "{text}");
    let rts_normalized = RuleTreeSet::<Normalized>::build_from(rts.clone());
    let rts_normalized_err = RuleTreeSet::<Normalized>::try_build_from(rts);
    let text = format!("rts_normalized errors: {}, err: {}", rts_normalized.get_log().num_errors(), rts_normalized_err.is_err());
    assert!(rts_normalized.get_log().num_errors() > 0, "{text}");
    assert!(rts_normalized_err.is_err(), "{text}");
    let prs = ProdRuleSet::build_from(rts_normalized.clone());
    let prs_e = ProdRuleSet::try_build_from(rts_normalized);
    let text = format!("prs errors: {}, err: {}", prs.get_log().num_errors(), prs_e.is_err());
    assert!(prs.get_log().num_errors() > 0, "{text}");
    assert!(prs_e.is_err(), "{text}");
}

pub(crate) fn complete_symbol_table(symbol_table: &mut SymbolTable, num_t: usize, num_nt: usize, is_t_data: bool) {
    if symbol_table.get_num_t() == 0 {
        assert!(num_t <= 26);
        symbol_table.extend_terminals((0..num_t).map(|i| (format!("{}", char::from(i as u8 + 97)),
                                                          if is_t_data { None } else { Some(format!("{}", char::from(i as u8 + 97))) })));
    }
    if symbol_table.get_num_nt() == 0 {
        assert!(num_nt <= 26);
        symbol_table.extend_nonterminals((0..num_nt as u8).map(|i| format!("{}", char::from(i + 65))));
    }
}
