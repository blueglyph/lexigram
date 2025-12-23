// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

/// Verifies the coherency of different symbol representations ([Symbol](crate::parser::Symbol), [OpCode](crate::parser::OpCode),
/// [SymbolTable](crate::SymbolTable), [FixedSymTable](crate::FixedSymTable), trait [SymInfoTable](crate::SymInfoTable))
mod symbols_repr {
    use lexigram_core::{opcode, CollectJoin};
    use lexigram_core::fixed_sym_table::SymInfoTable;
    use crate::{columns_to_str, sym, SymbolTable};
    use crate::parser::Symbol;
    use crate::parser::OpCode;

    fn get_symbol_table() -> SymbolTable {
        let mut symtable = SymbolTable::new();
        symtable.extend_terminals([
            ("Arrow".to_string(), Some("->".to_string())),
            ("Quote".to_string(), Some("\"".to_string())),
            ("Id".to_string(), None),
        ]);
        symtable.add_nonterminal("a");
        // symtable.extend_nonterminals((0_u8..26).map(|i| format!("{}", char::from(i + 97))));
        symtable
    }

    static SYMBOL_TESTS: [(Symbol, &str); 8] = [
        //               macro_item str    str_quote str_name str_ext        str/N str_quote/N str_name/N str_ext/N
        //              --------------------------------------------------------------------------------------------
        (sym!(t 0 ), r#" sym!(t 0)  ->     "->"      Arrow    ->             :0    :0          :0         :0        "#),
        (sym!(t 1 ), r#" sym!(t 1)  "      "\""      Quote    "              :1    :1          :1         :1        "#),
        (sym!(t 2 ), r#" sym!(t 2)  Id     Id        Id       Id(parsed "s") :2    :2          :2         :2        "#),
        (sym!(t 3 ), r#" sym!(t 3)  T(3?)  T(3?)     T(3?)    T(3?)          :3    :3          :3         :3        "#),
        (sym!(nt 0), r#" sym!(nt 0) a      a         a        a              0     0           0          0         "#),
        (sym!(nt 1), r#" sym!(nt 1) NT(1?) NT(1?)    NT(1?)   NT(1?)         1     1           1          1         "#),
        (sym!(e   ), r#" sym!(e)    ε      ε         ε        ε              ε     ε           ε          ε         "#),
        (sym!(end ), r#" sym!(end)  $      $         $        $              $     $           $          $         "#),
    ];

    static OPCODE_TESTS: [(OpCode, &str); 10] = [
        //                    macro_item      str     str_quote str_ext         str/N str_quote/N str_ext/N
        //                   -------------------------------------------------------------------------------
        (opcode!(e     ), r#" opcode!(e)      ε       ε         ε               ε     ε           ε         "#),
        (opcode!(t 0   ), r#" opcode!(t 0)    ->      "->"      ->              :0    :0          :0        "#),
        (opcode!(t 1   ), r#" opcode!(t 1)    "       "\""      "               :1    :1          :1        "#),
        (opcode!(t 2   ), r#" opcode!(t 2)    Id!     Id!       Id!(parsed "s") :2    :2          :2        "#),
        (opcode!(t 3   ), r#" opcode!(t 3)    T(3?)   T(3?)     T(3?)           :3    :3          :3        "#),
        (opcode!(nt 0  ), r#" opcode!(nt 0)   ►a      ►a        ►a              ►0    ►0          ►0        "#),
        (opcode!(nt 2  ), r#" opcode!(nt 2)   ►NT(2?) ►NT(2?)   ►NT(2?)         ►2    ►2          ►2        "#),
        (opcode!(loop 0), r#" opcode!(loop 0) ●a      ●a        ●a              ●0    ●0          ●0        "#),
        (opcode!(exit 0), r#" opcode!(exit 0) ◄0      ◄0        ◄0              ◄0    ◄0          ◄0        "#),
        (opcode!(end   ), r#" opcode!(end)    $       $         $               $     $           $         "#),
    ];

    static SYMINFOTABLE_TESTS: [(Symbol, &str); 6] = [
        //               get_t_str get_t_name get_nt_name get_name get_str get_name_quote
        //              ------------------------------------------------------------------
        (sym!(t 0 ), r#" ->        Arrow                  Arrow    ->      "->"           "#),
        (sym!(t 1 ), r#" "         Quote                  Quote    "       "\""           "#),
        (sym!(t 2 ), r#" Id        Id                     Id       Id      Id             "#),
        (sym!(t 3 ), r#" T(3?)     T(3?)                  T(3?)    T(3?)   T(3?)          "#),
        (sym!(nt 0), r#"                      a           a        a       a              "#),
        (sym!(nt 1), r#"                      NT(1?)      NT(1?)   NT(1?)  NT(1?)         "#),
    ];

    #[test]
    fn symbol_to_str() {
        const VERBOSE: bool = false;
        let symtable = get_symbol_table();
        let st: Option<&SymbolTable> = Some(&symtable);
        let no_st: Option<&SymbolTable> = None;
        let ext_string = "parsed \"s\"".to_string();
        let mut symbols = vec![];
        let mut cols = vec![vec![
            String::new(),
            "macro_item".to_string(),
            "str".to_string(),
            "str_quote".to_string(),
            "str_name".to_string(),
            "str_ext".to_string(),
            "str/N".to_string(),
            "str_quote/N".to_string(),
            "str_name/N".to_string(),
            "str_ext/N".to_string(),
            String::new(),
        ]];
        let mut expecteds = vec![];
        for (symbol, expected) in SYMBOL_TESTS {
            symbols.push(symbol);
            cols.push(vec![
                String::new(),
                format!("sym!({})", symbol.to_macro_item()),
                symbol.to_str(st),
                symbol.to_str_quote(st),
                symbol.to_str_name(st),
                symbol.to_str_ext(st, &ext_string),
                symbol.to_str(no_st),
                symbol.to_str_quote(no_st),
                symbol.to_str_name(no_st),
                symbol.to_str_ext(no_st, &ext_string),
                String::new(),
            ]);
            expecteds.push(expected.to_string());
        }
        let lines = columns_to_str(cols, None);
        let (hdr, lines) = lines.split_at(1);
        let tab = "        ";
        if VERBOSE {
            println!(
                "Results:\n{tab}//              {}\n{tab}//              {:-<w$}\n{}",
                hdr[0], "",
                symbols.into_iter().zip(lines).map(|(s, str)| format!("{tab}(sym!({:4}), r#\"{str}\"#),", s.to_macro_item())).join("\n"),
                w=hdr[0].len()
            );
        }
        for (result, expected) in lines.into_iter().zip(expecteds) {
            assert_eq!(result, &expected);
        }
    }

    #[test]
    fn opcode_to_str() {
        const VERBOSE: bool = false;
        let symtable = get_symbol_table();
        let st: Option<&SymbolTable> = Some(&symtable);
        let no_st: Option<&SymbolTable> = None;
        let ext_string = "parsed \"s\"".to_string();
        let mut symbols = vec![];
        let mut cols = vec![vec![
            String::new(),
            "macro_item".to_string(),
            "str".to_string(),
            "str_quote".to_string(),
            "str_ext".to_string(),
            "str/N".to_string(),
            "str_quote/N".to_string(),
            "str_ext/N".to_string(),
            String::new(),
        ]];
        let mut expecteds = vec![];
        for (opcode, expected) in OPCODE_TESTS {
            symbols.push(opcode);
            cols.push(vec![
                String::new(),
                format!("opcode!({})", opcode.to_macro_item()),
                opcode.to_str(st),
                opcode.to_str_quote(st),
                opcode.to_str_ext(st, &ext_string),
                opcode.to_str(no_st),
                opcode.to_str_quote(no_st),
                opcode.to_str_ext(no_st, &ext_string),
                String::new(),
            ]);
            expecteds.push(expected.to_string());
        }
        let lines = columns_to_str(cols, None);
        let (hdr, lines) = lines.split_at(1);
        let tab = "        ";
        if VERBOSE {
            println!(
                "Results:\n{tab}//                   {}\n{tab}//                   {:-<w$}\n{}",
                hdr[0], "",
                symbols.into_iter().zip(lines).map(|(s, str)| format!("{tab}(opcode!({:6}), r#\"{str}\"#),", s.to_macro_item())).join("\n"),
                w=hdr[0].len()
            );
        }
        for (result, expected) in lines.into_iter().zip(expecteds) {
            assert_eq!(result, &expected);
        }
    }

    #[test]
    fn symbol_table_to_str() {
        const VERBOSE: bool = false;
        let symtab = get_symbol_table();
        let fixedtable = symtab.clone().to_fixed_sym_table();
        let v: Vec<(&str, Box<dyn SymInfoTable>)> = vec![("SymbolTable", Box::new(symtab)), ("FixedSymTable", Box::new(fixedtable))];
        for (st_name, st) in v {
            let mut symbols = vec![];
            let mut cols = vec![vec![
                String::new(),
                "get_t_str".to_string(),
                "get_t_name".to_string(),
                "get_nt_name".to_string(),
                "get_name".to_string(),
                "get_str".to_string(),
                "get_name_quote".to_string(),
                String::new(),
            ]];
            let mut expecteds = vec![];
            for (symbol, expected) in SYMINFOTABLE_TESTS {
                symbols.push(symbol);
                cols.push(vec![
                    String::new(),
                    if let Symbol::T(tok) = symbol { st.get_t_str(tok) } else { String::new() },
                    if let Symbol::T(tok) = symbol { st.get_t_name(tok) } else { String::new() },
                    if let Symbol::NT(var) = symbol { st.get_nt_name(var) } else { String::new() },
                    st.get_name(&symbol),
                    st.get_str(&symbol),
                    st.get_name_quote(&symbol),
                    String::new(),
                ]);
                expecteds.push(expected.to_string());
            }
            let lines = columns_to_str(cols, None);
            let (hdr, lines) = lines.split_at(1);
            let tab = "        ";
            if VERBOSE {
                println!("{st_name}:");
                println!(
                    "{tab}//              {}\n{tab}//              {:-<w$}\n{}",
                    hdr[0], "",
                    symbols.into_iter().zip(lines).map(|(s, str)| format!("{tab}(sym!({:4}), r#\"{str}\"#),", s.to_macro_item())).join("\n"),
                    w = hdr[0].len()
                );
            }
            for (result, expected) in lines.into_iter().zip(expecteds) {
                assert_eq!(result, &expected);
            }
        }
    }
}