// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

mod ll1;
mod lr;

mod gen_integration {
    use lexigram_core::CollectJoin;
    use crate::grammar::ProdRuleSet;
    use crate::{LL1, LR};
    use crate::grammar::tests::TestRules;
    use lexigram_core::log::{LogReader, LogStatus};
    use crate::build::BuildFrom;
    use crate::parsergen::{ParserGen, ParserType};
    use crate::file_utils::{get_tagged_source, replace_tagged_source};

    fn get_source(tr_id: u32, indent: usize, include_alts: bool, gen_wrapper: bool, parser_type: ParserType, name: String) -> String {
        const VERBOSE: bool = false;

        let rules = TestRules(tr_id).to_prs_general().expect(&format!("invalid test rule ID #{tr_id}"));
        assert_eq!(rules.get_log().num_errors(), 0, "building {tr_id} failed:\n- {}", rules.get_log().get_errors().join("\n- "));
        let mut builder = match parser_type {
            ParserType::LL1 => {
                let ll1 = ProdRuleSet::<LL1>::build_from(rules);
                ParserGen::build_from_rules_ll1(ll1, name)
            }
            ParserType::LALR => {
                let lalr = ProdRuleSet::<LR>::build_from(rules);
                ParserGen::build_from_rules_lr(lalr, name)
            }
        };
        builder.set_include_alts(include_alts);
        builder.use_full_lib(true);
        builder.set_gen_wrapper(gen_wrapper);
        builder.set_indent(indent);
        let src = builder.gen_source_code().0;
        if VERBOSE {
            println!("{}", builder.log);
        }
        src
    }

    fn get_test_data<'a>(id: u32) -> Option<(u32, usize, bool, bool, ParserType, &'a str, &'a str)> {
        match id {
            // those parsers are also used in other tests:
            //        rules ind. alts   wrapper type              tag name                                        name
            1 => Some(( 580, 4,  true,  false,  ParserType::LL1,  "write_source_code_for_integration_listener1",  "Expr")),
            2 => Some(( 640, 4,  true,  false,  ParserType::LL1,  "write_source_code_for_integration_listener2",  "Expr")),
            3 => Some(( 641, 4,  true,  false,  ParserType::LL1,  "write_source_code_for_integration_listener3",  "Expr")),
            4 => Some(( 642, 4,  true,  false,  ParserType::LL1,  "write_source_code_for_integration_listener4",  "Expr")),
            5 => Some(( 862, 4,  true,  false,  ParserType::LL1,  "write_source_code_for_integration_listener5",  "Expr")),
            6 => Some(( 120, 4,  false, false,  ParserType::LL1,  "write_source_code_for_integration_listener6",  "Expr")),
            7 => Some(( 122, 4,  false, true,   ParserType::LALR, "write_source_code_for_integration_listener7",  "Test")),
            _ => None
        }
    }

    #[derive(Debug, Clone, Copy)]
    enum Action { VerifySource, WriteSource }
    #[derive(Debug, Clone, Copy)]
    enum SourceTestError { NoSuchTest, SourceNotFound, SourceDiffer }

    fn do_test(id: u32, action: Action, verbose: bool) -> Result<(), SourceTestError> {
        const FILENAME: &str = "tests/integration/parser_examples.rs";
        if let Some((tr_id, indent, include_alts, gen_wrapper, parser_type, tag, name)) = get_test_data(id) {
            let source = get_source(tr_id, indent, include_alts, gen_wrapper, parser_type, name.to_string());
            if verbose {
                let s = String::from_utf8(vec![32; indent]).unwrap();
                println!("{s}// [{tag}]\n{source}{s}// [{tag}]");
            }
            match action {
                Action::VerifySource => {
                    let result = get_tagged_source(FILENAME, tag).map_err(|e| {
                        println!("source not found for {id} / {tr_id} / {tag} ({name}): {e}");
                        SourceTestError::SourceNotFound
                    })?;
                    if result != source {
                        println!("source mismatch for {id} / {tr_id} / {tag} ({name})");
                        return Err(SourceTestError::SourceDiffer);
                    }
                }
                Action::WriteSource => {
                    replace_tagged_source(FILENAME, tag, source.as_str()).expect(&format!("couldn't write {FILENAME} / {tag}"));
                }
            }
            Ok(())
        } else {
            Err(SourceTestError::NoSuchTest)
        }
    }

    #[test]
    #[cfg(not(miri))]
    fn verify_integration_sources() {
        let mut errors = vec![];
        for i in 1_u32.. {
            match do_test(i, Action::VerifySource, false) {
                Err(SourceTestError::NoSuchTest) => break,
                Err(_) => errors.push(i),
                Ok(_) => {}
            }
        }
        if !errors.is_empty() { panic!("verification failed with {} error(s): {}", errors.len(), errors.into_iter().map(|n| n.to_string()).join(", ")); }
    }

    #[ignore]
    #[test]
    #[cfg(not(miri))]
    fn write_all_sources() {
        for i in 1_u32.. {
            match do_test(i, Action::WriteSource, false) {
                Ok(_) => println!("writing source for test {i}"),
                Err(SourceTestError::NoSuchTest) => break,
                Err(e) => panic!("error while writing source for test {i}: {e:?}"),
            }
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener1() {
        do_test(1, Action::WriteSource, true).expect("couldn't write source #1");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener2() {
        do_test(2, Action::WriteSource, true).expect("couldn't write source #2");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener3() {
        do_test(3, Action::WriteSource, true).expect("couldn't write source #3");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener4() {
        do_test(4, Action::WriteSource, true).expect("couldn't write source #4");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener5() {
        do_test(5, Action::WriteSource, true).expect("couldn't write source #5");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener6() {
        do_test(6, Action::WriteSource, true).expect("couldn't write source #6");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener7() {
        do_test(7, Action::WriteSource, true).expect("couldn't write source #7");
    }
}

mod parser_source {
    use crate::grammar::tests::TestRules;
    use lexigram_core::log::{LogReader, LogStatus};
    use crate::build::BuildFrom;
    use crate::parsergen::{ParserGen, LLParserTables};

    #[test]
    fn alternatives() {
        for include_alts in [false, true] {
            let ll1 = TestRules(900).to_prs_ll1().unwrap();
            assert_eq!(ll1.get_log().num_errors(), 0, "building the LL(1) failed:\n{}", ll1.get_log());
            let mut builder = ParserGen::build_from_rules_ll1(ll1, "simple".to_string());
            builder.set_include_alts(include_alts);
            builder.set_gen_wrapper(false);
            let (src, ..) = builder.gen_source_code();
            let alt_present = src.contains("static ALTERNATIVES");
            assert_eq!(alt_present, include_alts, "unexpected source code: include_alts = {include_alts}, code = \n{src}");
            let pt = LLParserTables::build_from(builder);
            let parser = pt.make_parser();
            let alts = parser.get_alts();
            assert_eq!(alts.is_empty(), !include_alts, "unexpected: include_alts = {include_alts}, alts = {alts:?}");
        }
    }
}

pub(super) mod wrapper_source {
    use std::collections::{BTreeMap, HashMap};
    use iter_index::IndexerIterator;
    use lexigram_core::alt::alt_to_rule_str;
    use crate::grammar::tests::TestRules;
    use crate::parser::Symbol;
    use crate::{columns_to_str, indent_source, AltId, VarId};
    use crate::parsergen::{NTValue, ParserGen, ParserType};
    use lexigram_core::log::{LogReader, LogStatus};
    use crate::parsergen::SpanNbr;
    use crate::file_utils::{get_tagged_source, replace_tagged_source};
    use lexigram_core::CollectJoin;
    use lexigram_core::parser::OpCode;

    /// fields of each test in [build_items()]
    pub type BuildItemsTestEntry = (
        u32,                                        // TestRules #
        bool,                                       // test sources?
        bool,                                       // test sources include parser?
        bool,                                       // use super::super::wrapper_code::...?
        u16,                                        // start NT
        BTreeMap<VarId, String>,                    // NT types
        Vec<(Vec<OpCode>, SpanNbr, Vec<Symbol>)>,   // expected opcodes, span, items for each alt
        NTValue,                                    // which symbols have a value
        BTreeMap<VarId, Vec<AltId>>,                // expected alt groups
    );

    /// test specifications for [build_items()]
    pub struct BuildItemsTestSpec<'a> {
        pub enable_test_source: bool,
        pub tests_all: bool,
        pub replace_source: bool,
        pub parser_type: ParserType,
        pub wrapper_filename: &'a str,
        pub tests: Vec<BuildItemsTestEntry>
    }

    #[allow(unused_doc_comments)]
    #[allow(unused_variables)]
    #[allow(unused_mut)]
    /// Tests [ParserGen::source_build_parser], [ParserGen::source_wrapper], [ParserGen::source_use], and [ParserGen::calc_item_ops].
    pub fn build_items(spec: BuildItemsTestSpec) {
        let BuildItemsTestSpec {
            mut enable_test_source,
            mut tests_all,
            mut replace_source,
            parser_type,
            wrapper_filename,
            tests
        } = spec;

        // print sources
        const VERBOSE: bool = false;        // prints the `tests` values from the results (easier to set the other constants to false)
        const VERBOSE_LOG: bool = false;     // always prints the log
        const VERBOSE_TYPE: bool = false;   // prints the code module skeleton (easier to set the other constants to false)
        const PRINT_SOURCE: bool = false;   // prints the wrapper module (easier to set the other constants to false)
        const SHOW_ANSWER: bool = false;

        // override options
        // enable_test_source = true;
        // tests_all = true;

        // CAUTION! Setting this to 'true' modifies the validation file with the current result
        // replace_source = false;

        let mut num_errors = 0;
        let mut num_src_errors = 0;
        let mut rule_id_iter = HashMap::<u32, u32>::new();
        for (test_id, test_entry) in tests.into_iter().enumerate() {
            let (
                tr_id,
                test_source, test_source_parser, use_wrapper_code,
                start_nt, nt_type,
                expected_items, has_value, expected_alts
            ) = test_entry;
            // if !matches!(tr_id, 109|120) { continue }
            let rule_iter = rule_id_iter.entry(tr_id).and_modify(|x| *x += 1).or_insert(1);
            if VERBOSE { println!("// {:=<80}\n// Test {test_id}: TestRule({tr_id}) #{rule_iter}, start {start_nt}:", ""); }
            let (mut builder, original_str) = match parser_type {
                ParserType::LL1 => {
                    let ll1_maybe = TestRules(tr_id).to_prs_ll1_with_start(start_nt);
                    if ll1_maybe.is_none() { continue }
                    let ll1 = ll1_maybe.unwrap();
                    let symtab = ll1.get_symbol_table();
                    if VERBOSE {
                        println!("/*");
                        symtab.unwrap().dump("symbol table:");
                        println!("Terminals: {}", ll1.get_symbol_table().unwrap()
                            .get_terminals().enumerate()
                            .map(|(i, (s1, s2))| format!("{i}:{s1}{}", if let Some(s2t) = s2 { format!("=\"{s2t}\"") } else { String::new() })).join(", "));
                        println!("LL1 <-> origin:\n{}", indent_source(vec![ll1.prs_alt_origins_str()], 4));
                    }
                    if !ll1.has_no_errors() {
                        if VERBOSE {
                            println!("## LL(1) build errors:\n{}", ll1.get_log());
                            num_errors += 1;
                            continue;
                        }
                    }
                    let original_str = ll1.get_original_str(8);
                    (ParserGen::build_from_rules_ll1(ll1, "Test".to_string()), original_str)
                }
                ParserType::LALR => {
                    let lr_maybe = TestRules(tr_id).to_prs_lr_with_start(start_nt);
                    let Some(lr) = lr_maybe else { continue };
                    let original_str = lr.get_original_str(8);
                    (ParserGen::build_from_rules_lr(lr, "Test".to_string()), original_str)
                }
            };
            builder.set_gen_span_params(true);
            builder.set_include_alts(true);
            builder.use_full_lib(true);
            let ambig_warnings = builder.log.get_warnings().filter(|w| w.get_inner_str().contains("calc_table: ambiguity")).join("\n");
            let result_is_ambiguous = !ambig_warnings.is_empty();
            builder.set_nt_value(has_value.clone());
            if VERBOSE {
                println!("before, NT with value: {}",
                         (0..builder.num_nt).into_iter().filter_map(|v|
                             if builder.nt_values[v] { Some(Symbol::NT(v as VarId).to_str(builder.get_symbol_table())) } else { None }
                         ).join(", "));
            }
            builder.set_indent(4);
            let test_name = format!("wrapper source for rule {tr_id} #{rule_iter}, start {}", Symbol::NT(0).to_str(builder.get_symbol_table()));
            let rule_name = format!("{tr_id}_{rule_iter}");
            if use_wrapper_code {
                builder.add_lib(&format!("super::super::wrapper_code::code_{rule_name}::*"));
            }
            for (v, s) in nt_type.clone() {
                builder.add_nt_type(v, s);
            }
            builder.set_gen_parser(test_source_parser);
            let result_nt_type = builder.nt_type.iter().map(|(v, s)| (*v, s.clone())).collect::<BTreeMap<_, _>>();
            let (result_src, ..) = builder.gen_source_code();
            if VERBOSE {
                println!("after,  NT with value: {}",
                         (0..builder.num_nt).into_iter().filter_map(|v|
                             if builder.nt_values[v] { Some(Symbol::NT(v as VarId).to_str(builder.get_symbol_table())) } else { None }
                         ).join(", "));
            }
            let result_items = builder.item_ops.iter().enumerate()
                .map(|(a_id, v)| (builder.opcodes[a_id].clone(), builder.span_nbrs[a_id], v.clone()))
                .to_vec();
            let result_alts = (0..builder.num_nt).filter_map(|v|
                if builder.parent[v].is_none() { Some((v as VarId, builder.gather_alts(v as VarId))) } else { None }
            ).collect::<BTreeMap<_, _>>();
            if VERBOSE {
                let gather_alts = (0..(builder.num_nt as VarId)).map(|v| (v, builder.gather_alts(v))).to_vec();
                println!("gather_alts:\n{}", gather_alts.iter().map(|(v, alts)| {
                    format!(
                        "- {} -> {}",
                        Symbol::NT(*v).to_str(builder.get_symbol_table()),
                        alts.iter().map(|a| {
                            let (va, _) = builder.alts[*a as usize];
                            format!("({}: {a})", Symbol::NT(va).to_str(builder.get_symbol_table()))
                        }).join(", "))
                }).join("\n"));
            }
            if VERBOSE || SHOW_ANSWER {
                if *rule_iter == 1 {
                    if !VERBOSE { println!(); }
                    println!("{original_str}");
                    println!("        //");
                    let infos = builder.nt_info_str();
                    println!("{}", infos.into_iter().map(|s| format!("        // {s}")).join("\n"));
                } else {
                    println!(
                        "        // {}",
                        builder.symbol_table.get_nonterminals().enumerate()
                            .map(|(nt, s)| format!("{s}: {}", if builder.nt_values[nt] { 'y' } else { 'n' })).join(", "));
                }
                println!("        ({tr_id}, {test_source}, {test_source_parser}, {use_wrapper_code}, {start_nt}, btreemap![", );
                if !result_nt_type.is_empty() {
                    println!("{}", result_nt_type.iter().map(|(v, s)| format!("            {v} => \"{s}\".to_string(),")).join("\n"));
                }
                println!("        ], vec![");
                builder.print_items(12, true);
                let has_value_str = match &has_value {
                    NTValue::SetIds(s) => format!("NTValue::SetIds(vec![{}])", s.iter().map(|s| s.to_string()).join(", ")),
                    NTValue::SetNames(s) => format!("NTValue::SetNames(vec![{}])", s.iter().map(|s| format!("{s:?}")).join(", ")),
                    NTValue::Parents | NTValue::Default | NTValue::None => format!("NTValue::{has_value:?}"),
                };
                println!("        ], {has_value_str}, btreemap![{}]),",
                    if result_alts.is_empty() { "".to_string() } else { result_alts.iter().map(|(v, a)| format!("{v} => vec![{}]", a.iter().join(", "))).join(", ") }
                );
            }
            if VERBOSE {
                let nbr_alts = builder.alts.len();
                let original = (0..nbr_alts)
                    .filter_map(|i| builder.get_original_alt_str(i as AltId, builder.get_symbol_table()).and_then(|s| Some(format!("- {i:3}: {s}"))))
                    .join("\n");
                if !original.is_empty() {
                    println!("Original alts:\n{original}");
                }
                println!("*/");
            }
            let builder_has_errors = builder.log.num_errors() > 0;
            if VERBOSE && builder_has_errors || VERBOSE_LOG {
                println!("Log:\n{}", builder.log);
            }
            if VERBOSE_TYPE {
                if result_is_ambiguous {
                    println!("parsing table has ambiguities:\n{ambig_warnings}")
                }
                if builder_has_errors {
                    println!("builder couldn't generate the source");
                }
            }
            if PRINT_SOURCE && !builder_has_errors {
                println!("pub(crate) mod rules_{rule_name} {{");
                println!("    // {0:-<60}\n    // [{test_name}]\n\n{result_src}\n    // [{test_name}]\n    // {:-<60}", "");
                println!("}}\n");
            }
            if VERBOSE {
                println!("tag:     [{test_name}]");
                println!("code:     code_{rule_name}");
            }
            let expected_src = if test_source && !cfg!(miri) {
                let src = get_tagged_source(wrapper_filename, &test_name);
                if (enable_test_source || replace_source) && src.is_err() {
                    println!("## couldn't find the source code: {}", src.as_ref().err().unwrap());
                }
                src.ok()
            } else {
                None
            };
            let err_msg = format!("test {test_id} TestRules({tr_id}) #{rule_iter} failed");
            if tests_all {
                if result_items != expected_items || result_alts != expected_alts || result_nt_type != nt_type {
                    num_errors += 1;
                    println!(
                        "## ERROR: {err_msg}{}{}{}",
                        if result_items != expected_items { ", items mismatch" } else { "" },
                        if result_alts != expected_alts { ", alts mismatch" } else { "" },
                        if result_nt_type != nt_type { ", result type mismatch" } else { "" });
                }
                if (test_source && !cfg!(miri) && enable_test_source && Some(&result_src) != expected_src.as_ref()) || builder_has_errors {
                    if builder_has_errors {
                        println!("## ERRORS WHILE GENERATING SOURCE: {err_msg}");
                    } else {
                        if replace_source {
                            if replace_tagged_source(wrapper_filename, &test_name, &result_src).is_err() {
                                num_errors += 1;
                                println!("## ERROR: {err_msg}, couldn't replace source");
                            }
                        }
                        println!("## SOURCE MISMATCH: {err_msg}");
                    }
                    num_errors += 1;
                    num_src_errors += 1;
                }
                if result_is_ambiguous {
                    println!("## ERROR: {err_msg}, parsing table had ambiguities:\n{ambig_warnings}");
                }
            } else {
                assert_eq!(result_items, expected_items, "{err_msg}, different items");
                assert_eq!(result_alts, expected_alts, "{err_msg}, different alts");
                assert_eq!(result_nt_type, nt_type, "{err_msg}, different NT types");
                if !cfg!(miri) && enable_test_source && test_source {
                    assert!(!builder_has_errors, "{} errors reported by source builder", builder.log.num_errors());
                    if replace_source && expected_src.is_some() && &result_src != expected_src.as_ref().unwrap() && !builder_has_errors {
                        replace_tagged_source(wrapper_filename, &test_name, &result_src).expect("replacement failed");
                    }
                    assert_eq!(Some(result_src), expected_src, "{err_msg}");
                }
                assert!(!result_is_ambiguous, "{err_msg}, parsing table had ambiguities:\n{ambig_warnings}");
            }
        }
        if tests_all {
            assert_eq!(num_errors, 0, "{num_errors} test(s) have failed, including {num_src_errors} source error(s)");
        }
    }

    #[test]
    /// Tests [ParserGen::full_alt_str].
    fn expand_lfact() {
        let tests: Vec<(u32, Vec<Option<&str>>)> = vec![
            // a -> A | B
            (2, vec![
                Some(r#"a -> A"#),                // 0: a -> A
                Some(r#"a -> B"#),                // 1: a -> B
            ]),
            // a -> A B* C
            (102, vec![
                Some(r#"a -> A B* C"#),                       // 0: a -> A a_1 C
                Some(r#"`B` item in `a -> A  ►► B ◄◄ * C`"#), // 1: a_1 -> B a_1
                None,                                         // 2: a_1 -> ε
            ]),
            // a -> A B+ C
            (103, vec![
                Some(r#"a -> A B+ C"#),                       // 0: a -> A a_1 C
                Some(r#"`B` item in `a -> A  ►► B ◄◄ + C`"#), // 1: a_1 -> B a_2
                Some(r#"`B` item in `a -> A  ►► B ◄◄ + C`"#), // 2: a_2 -> a_1
                Some(r#"`B` item in `a -> A  ►► B ◄◄ + C`"#), // 3: a_2 -> ε
            ]),
            // a -> A (<L=i> B)+ C
            (201, vec![
                Some(r#"a -> A (<L> B)+ C"#),                                // 0: a -> A i C
                Some(r#"`<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`"#), // 1: i -> B a_1
                Some(r#"`<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`"#), // 2: a_1 -> i
                Some(r#"`<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`"#), // 3: a_1 -> ε
            ]),
            // a -> (<L=i> b A b B A)*
            // b -> C
            (202, vec![
                Some(r#"a -> (<L> b A b B A)*"#),                                        // 0: a -> i
                Some(r#"`<L> b A b B A` iteration in `a -> ( ►► <L> b A b B A ◄◄ )*`"#), // 1: i -> b A b B A i
                None,                                                                    // 2: i -> ε
                Some(r#"b -> C"#),                                                       // 3: b -> C
            ]),
            // a -> (<L=i> A (<L=j> b ",")* ";")* C
            // b -> B
            (208, vec![
                Some(r#"a -> (<L> A (<L> b ",")* ";")* C"#),                                                 // 0: a -> i C
                Some(r#"`<L> A (<L> b ",")* ";"` iteration in `a -> ( ►► <L> A (<L> b ",")* ";" ◄◄ )* C`"#), // 1: i -> A j ";" i
                None,                                                                                        // 2: i -> ε
                Some(r#"`<L> b ","` iteration in `a -> (<L> A ( ►► <L> b "," ◄◄ )* ";")* C`"#),              // 3: j -> b "," j
                None,                                                                                        // 4: j -> ε
                Some(r#"b -> B"#),                                                                           // 5: b -> B
            ]),
            // a -> (<L=i> A (<L=j> b ",")+ ";")+ C
            // b -> B
            (209, vec![
                Some(r#"a -> (<L> A (<L> b ",")+ ";")+ C"#),                                                 // 0: a -> i C
                Some(r#"`<L> A (<L> b ",")+ ";"` iteration in `a -> ( ►► <L> A (<L> b ",")+ ";" ◄◄ )+ C`"#), // 1: i -> A j ";" a_1
                Some(r#"`<L> b ","` iteration in `a -> (<L> A ( ►► <L> b "," ◄◄ )+ ";")+ C`"#),              // 2: j -> b "," a_2
                Some(r#"b -> B"#),                                                                           // 3: b -> B
                Some(r#"`<L> A (<L> b ",")+ ";"` iteration in `a -> ( ►► <L> A (<L> b ",")+ ";" ◄◄ )+ C`"#), // 4: a_1 -> i
                Some(r#"`<L> A (<L> b ",")+ ";"` iteration in `a -> ( ►► <L> A (<L> b ",")+ ";" ◄◄ )+ C`"#), // 5: a_1 -> ε
                Some(r#"`<L> b ","` iteration in `a -> (<L> A ( ►► <L> b "," ◄◄ )+ ";")+ C`"#),              // 6: a_2 -> j
                Some(r#"`<L> b ","` iteration in `a -> (<L> A ( ►► <L> b "," ◄◄ )+ ";")+ C`"#),              // 7: a_2 -> ε
            ]),
            // a -> (<L=i> A | B)*
            (250, vec![
                Some(r#"a -> (<L> A | B)*"#),                                // 0: a -> i
                Some(r#"`<L> A` iteration in `a -> ( ►► <L> A ◄◄  | B)*`"#), // 1: i -> A i
                Some(r#"`B` iteration in `a -> (<L> A |  ►► B ◄◄ )*`"#),     // 2: i -> B i
                None,                                                        // 3: i -> ε
            ]),
            // expr -> Id "." expr | "(" Num ")"
            (301, vec![
                Some(r#"expr -> Id "." expr"#),   // 0: expr -> Id "." expr
                Some(r#"expr -> "(" Num ")""#),   // 1: expr -> "(" Num ")"
            ]),
            // expr -> <L=expr> Id "." expr | "(" Num ")"
            (401, vec![
                Some(r#"expr -> <L> Id "." expr"#), // 0: expr -> Id "." expr
                Some(r#"expr -> "(" Num ")""#),     // 1: expr -> "(" Num ")"
            ]),
            // a -> a "!" | "?"
            (500, vec![
                Some(r#"a -> "?""#),              // 0: a -> "?" a_1
                Some(r#"a -> a "!""#),            // 1: a_1 -> "!" a_1
                None,                             // 2: a_1 -> ε
            ]),
            // a -> a "b" | a "c" | "a"
            (501, vec![
                Some(r#"a -> "a""#),              // 0: a -> "a" a_1
                Some(r#"a -> a "b""#),            // 1: a_1 -> "b" a_1
                Some(r#"a -> a "c""#),            // 2: a_1 -> "c" a_1
                None,                             // 3: a_1 -> ε
            ]),
            // e -> e "!" | "-" e | Num
            (580, vec![
                Some(r#"e -> "-" e"#),            // 0: e -> "-" e
                Some(r#"e -> Num"#),              // 1: e -> Num e_1
                Some(r#"e -> e "!""#),            // 2: e_1 -> "!" e_1
                None,                             // 3: e_1 -> ε
            ]),
            // e -> e "*" e | e "+" e | "!" e | Num
            (603, vec![
                None,                             // 0: e -> e_4 e_1
                Some(r#"e -> e "*" e"#),          // 1: e_1 -> "*" e_4 e_1
                Some(r#"e -> e "+" e"#),          // 2: e_1 -> "+" e_2 e_1
                None,                             // 3: e_1 -> ε
                None,                             // 4: e_2 -> e_4 e_3
                Some(r#"e -> e "*" e"#),          // 5: e_3 -> "*" e_4 e_3
                None,                             // 6: e_3 -> ε
                Some(r#"e -> "!" e"#),            // 7: e_4 -> "!" e
                Some(r#"e -> Num"#),              // 8: e_4 -> Num
            ]),
            // e -> e "*" e | e "+" | "!" e | Num
            (630, vec![
                None,                             // 0: e -> e_2 e_1
                Some(r#"e -> e "*" e"#),          // 1: e_1 -> "*" e_2 e_1
                Some(r#"e -> e "+""#),            // 2: e_1 -> "+" e_1
                None,                             // 3: e_1 -> ε
                Some(r#"e -> "!" e"#),            // 4: e_2 -> "!" e
                Some(r#"e -> Num"#),              // 5: e_2 -> Num
            ]),
            // a -> A | A B
            (700, vec![
                None,                             // 0: a -> A a_1
                Some(r#"a -> A B"#),              // 1: a_1 -> B
                Some(r#"a -> A"#),                // 2: a_1 -> ε
            ]),
            // a -> A B C | B B C | B C | B B A
            (704, vec![
                Some(r#"a -> A B C"#),            // 0: a -> A B C
                None,                             // 1: a -> B a_1
                None,                             // 2: a_1 -> B a_2
                Some(r#"a -> B C"#),              // 3: a_1 -> C
                Some(r#"a -> B B A"#),            // 4: a_2 -> A
                Some(r#"a -> B B C"#),            // 5: a_2 -> C
            ]),
            // a -> A* B a | C
            (810, vec![
                Some(r#"a -> A* B a"#),                           // 0: a -> a_1 B a
                Some(r#"a -> C"#),                                // 1: a -> C
                Some(r#"`A` item in `a ->  ►► A ◄◄ * B a | C`"#), // 2: a_1 -> A a_1
                None,                                             // 3: a_1 -> ε
            ]),
            // a -> A+ B a | C
            (811, vec![
                Some(r#"a -> A+ B a"#),                           // 0: a -> a_1 B a
                Some(r#"a -> C"#),                                // 1: a -> C
                Some(r#"`A` item in `a ->  ►► A ◄◄ + B a | C`"#), // 2: a_1 -> A a_2
                Some(r#"`A` item in `a ->  ►► A ◄◄ + B a | C`"#), // 3: a_2 -> a_1
                Some(r#"`A` item in `a ->  ►► A ◄◄ + B a | C`"#), // 4: a_2 -> ε
            ]),
            // a -> a A* C | B
            (820, vec![
                Some(r#"a -> B"#),                                // 0: a -> B a_2
                Some(r#"`A` item in `a -> a  ►► A ◄◄ * C | B`"#), // 1: a_1 -> A a_1
                None,                                             // 2: a_1 -> ε
                Some(r#"a -> a A* C"#),                           // 3: a_2 -> a_1 C a_2
                None,                                             // 4: a_2 -> ε
            ]),
            // a -> a A+ C | B
            (821, vec![
                Some(r#"a -> B"#),                                // 0: a -> B a_2
                Some(r#"`A` item in `a -> a  ►► A ◄◄ + C | B`"#), // 1: a_1 -> A a_3
                Some(r#"a -> a A+ C"#),                           // 2: a_2 -> a_1 C a_2
                None,                                             // 3: a_2 -> ε
                Some(r#"`A` item in `a -> a  ►► A ◄◄ + C | B`"#), // 4: a_3 -> a_1
                Some(r#"`A` item in `a -> a  ►► A ◄◄ + C | B`"#), // 5: a_3 -> ε
            ]),
            // a -> a A | B C | B D
            (870, vec![
                None,                             // 0: a -> B a_2
                Some(r#"a -> a A"#),              // 1: a_1 -> A a_1
                None,                             // 2: a_1 -> ε
                Some(r#"a -> B C"#),              // 3: a_2 -> C a_1
                Some(r#"a -> B D"#),              // 4: a_2 -> D a_1
            ]),
            // a -> a A B | a A C | D
            (871, vec![
                Some(r#"a -> D"#),                // 0: a -> D a_1
                None,                             // 1: a_1 -> A a_2
                None,                             // 2: a_1 -> ε
                Some(r#"a -> a A B"#),            // 3: a_2 -> B a_1
                Some(r#"a -> a A C"#),            // 4: a_2 -> C a_1
            ]),
            /*
            (999, vec![
            ]),
            */
        ];
        const VERBOSE: bool = false;
        const VERBOSE_SOLUTION: bool = false;
        let mut rule_id_iter = HashMap::<u32, u32>::new();
        let mut errors = 0;
        for (test_id, (tr_id, expected_full)) in tests.into_iter().enumerate() {
            let rule_iter = rule_id_iter.entry(tr_id).and_modify(|x| *x += 1).or_insert(1);
            if VERBOSE { println!("// {:=<80}\n// Test {test_id}: rules {tr_id} #{rule_iter}:", ""); }

            let expected_full = expected_full.into_iter()
                .map(|opt| if let Some(s) = opt { format!("Some(r#\"{s}\"#)") } else { "None".to_string() })
                .to_vec();
            let ll1 = TestRules(tr_id).to_prs_ll1().unwrap();
            let original_str = ll1.get_original_str(12);
            let builder = ParserGen::build_from_rules_ll1(ll1, "Test".to_string());
            let symtable = builder.get_symbol_table();
            let mut result_full = vec![];
            for (a_id, (_v, a)) in builder.alts.iter().index() {
                result_full.push(
                    format!("{}", if let Some(s) = a.get_origin().and_then(|_| Some(builder.full_alt_str(a_id, None, false))) {
                        format!("Some(r#\"{s}\"#)")
                    } else {
                        "None".to_string()
                    }));
            }
            if VERBOSE_SOLUTION || VERBOSE {
                println!("{original_str}");
                println!("            ({tr_id}, vec![", );
                let cols = result_full.iter().enumerate()
                    .map(|(i, s_full)| {
                        let (v, prod) = &builder.alts[i];
                        vec![
                            "".to_string(),
                            format!("{s_full},"),
                            format!("// {i}: {}", alt_to_rule_str(*v, prod, symtable)),
                        ]
                    })
                    .to_vec();
                let lines = columns_to_str(cols, Some(vec![16, 34, 0]));
                println!("{}", lines.join("\n"));
                println!("            ]),");
            }
            if result_full != expected_full {
                errors += 1;
                if VERBOSE {
                    println!("## ERROR: {result_full:?} doesn't match {expected_full:?}")
                }
            }
        }
        assert_eq!(errors, 0);
    }
}
