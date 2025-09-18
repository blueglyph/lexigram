// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use crate::grammar::tests::rts::build_rts;
use super::*;

// ---------------------------------------------------------------------------------------------
// ProdRuleSet

impl<T> ProdRuleSet<T> {
    pub fn get_non_empty_nts(&self) -> impl Iterator<Item=(VarId, &ProdRule)> {
        self.get_prules_iter().filter(|(_, rule)| **rule != prule!(e))
    }
}

impl<T> ProdRuleSet<T> {
    fn new() -> Self {
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

    pub(crate) fn print_logs(&self) {
        println!("{}\n", self.log.get_messages_str());
    }
}

impl<T> From<&ProdRuleSet<T>> for BTreeMap<VarId, ProdRule> {
    fn from(rules: &ProdRuleSet<T>) -> Self {
        rules.get_prules_iter().map(|(var, p)| (var, p.clone())).collect::<BTreeMap<_, _>>()

    }
}

pub fn print_alts<T: SymInfoTable>(alts: &Vec<(VarId, Alternative)>, symbol_table: Option<&T>) {
    println!("alternatives:\n{}",
             alts.iter().enumerate().map(|(id, (v, f))|
                 format!("            // - {id}: {} -> {}{}",
                         Symbol::NT(*v).to_str(symbol_table),
                         f.to_str(symbol_table),
                         if f.get_flags() != 0 { format!("     {} ({})", ruleflag::to_string(f.get_flags()).join(" | "), f.get_flags()) } else { "".to_string() }
                 )
    ).join("\n"));
}

pub fn print_expected_code(result: &BTreeMap<VarId, ProdRule>) {
    println!("            {}", result.iter().map(|(i, p)|
        format!("{i} => prule!({}),", p.iter()
            .map(|f| format!("{}{}", if f.get_flags() != 0 { format!("#{}, ", f.get_flags()) } else { "".to_string() }, f.iter().map(|s| s.to_macro_item()).join(", ")))
            .join("; "))).join("\n            "))
}

fn map_and_print_first<'a>(first: &'a HashMap<Symbol, HashSet<Symbol>>, symbol_table: Option<&'a SymbolTable>) -> BTreeMap<&'a Symbol, BTreeSet<&'a Symbol>> {
    println!("first: ");
    let b = first.iter().map(|(s, hs)| (s, hs.iter().collect::<BTreeSet<_>>())).collect::<BTreeMap<_, _>>();
    for (sym, set) in &b {
        println!("// {} => {}", sym.to_str(symbol_table), set.iter().map(|s| s.to_str(symbol_table)).join(" "));
    }
    b
}

fn map_and_print_follow<'a>(follow: &'a HashMap<Symbol, HashSet<Symbol>>, symbol_table: Option<&'a SymbolTable>) -> BTreeMap<&'a Symbol, BTreeSet<&'a Symbol>> {
    println!("follow:");
    let b = follow.iter().map(|(s, hs)| (s, hs.iter().collect::<BTreeSet<_>>())).collect::<BTreeMap<_, _>>();
    for (sym, set) in &b {
        println!("// {} => {}", sym.to_str(symbol_table), set.iter().map(|s| s.to_str(symbol_table)).join(" "));
    }
    b
}

fn def_arith_symbols(symbol_table: &mut SymbolTable, has_term: bool) {
    symbol_table.extend_terminals([
        ("SUB".to_string(), Some("-".to_string())),
        ("ADD".to_string(), Some("+".to_string())),
        ("DIV".to_string(), Some("/".to_string())),
        ("MUL".to_string(), Some("*".to_string())),
        ("LPAREN".to_string(), Some("(".to_string())),
        ("RPAREN".to_string(), Some(")".to_string())),
        ("N".to_string(), None),
        ("I".to_string(), None)
    ]);
    symbol_table.extend_nonterminals(["E".to_string()]);
    if has_term {
        symbol_table.extend_nonterminals(["T".to_string()]);
    }
    symbol_table.extend_nonterminals(["F".to_string()]);
}

// ---------------------------------------------------------------------------------------------

pub(crate) fn build_prs(id: u32, is_t_data: bool) -> ProdRuleSet<General> {
    let mut rules = ProdRuleSet::new();
    let mut symbol_table = SymbolTable::new();
    let prules = &mut rules.prules;
    let mut start = Some(0);
    let flags = HashMap::<VarId, u32>::new();
    let parents = HashMap::<VarId, VarId>::new();   // (child, parent)
    let mut extend_nt = true;
    match id {
        // misc tests ----------------------------------------------------------
        0 => {
            prules.extend([
                prule!(nt 0, t 1; nt 0, t 2; t 3; t 3, t 4), // A -> A b | A c | d | d e
                prule!(nt 0, t 5; t 6; t 7),                 // B -> A f | g | h
            ]);
        }
        1 => {
            prules.extend([
                // A -> d c | b b c d | d c e | b d e g | b b c | c b | b c g | b d e f
                prule!(
                    t 3, t 2;
                    t 1, t 1, t 2, t 3;
                    t 3, t 2, t 4;
                    t 1, t 3, t 4, t 6;
                    #R, t 1, t 1, t 2;
                    t 2, t 1;
                    t 1, t 2, t 6;
                    t 1, t 3, t 4, t 5;
                )
            ]);
        }
        2 => { // tests empty prule
            prules.extend([]);
        }
        3 => { // tests continue / break consistency in left_factorize
            prules.extend([
                prule!(t 0),
                prule!(t 2, t 0; t 2),
                prule!(t 2; t 1),
                prule!(t 3)]);
        }
        4 => {
            // classical arithmetic grammar
            // T:  0:-, 1:+, 2:/, 3:*, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F
            def_arith_symbols(&mut symbol_table, true);
            prules.extend([
                prule!(nt 0, t 0, nt 1; nt 0, t 1, nt 1; nt 1),  // E -> E + T | E - T | T
                prule!(nt 1, t 2, nt 2; nt 1, t 3, nt 2; nt 2),  // T -> T * F | T / F | F
                prule!(t 4, nt 0, t 5; t 6; t 7),                // F -> ( E ) | NUM | ID
            ]);
        }
        5 => {
            // ε in first propagation
            symbol_table.extend_terminals([
                ("SUB".to_string(), Some("-".to_string())),
                ("ADD".to_string(), Some("+".to_string())),
                ("SEMI".to_string(), Some(";".to_string()))
            ]);
            symbol_table.extend_nonterminals(["A".to_string(), "A1".to_string(), "A2".to_string()]);
            prules.extend([
                prule!(nt 1, nt 2, t 2, t 2), // A -> A1 A2 ; ;
                prule!(t 0, nt 1; e),         // A1 -> - A1 | ε
                prule!(t 1, nt 2; e),         // A2 -> + A2 | ε
            ]);
        }
        6 => {
            // another starting NT
            symbol_table.extend_terminals([
                ("SUB".to_string(), Some("-".to_string())),
                ("ADD".to_string(), Some("+".to_string())),
                ("SEMI".to_string(), Some(";".to_string()))
            ]);
            symbol_table.extend_nonterminals(["A1".to_string(), "A".to_string(), "A2".to_string()]);
            prules.extend([
                prule!(t 0, nt 0; e),    // A1 -> - A1 | ε
                prule!(nt 0, nt 2, t 2), // A -> A1 A2 ;     <-- start
                prule!(t 1, nt 2; e),    // A2 -> + A2 | ε
            ]);
            start = Some(1);
        }
        7 => {
            // ε in first propagation
            symbol_table.extend_terminals([
                ("START".to_string(), Some(">".to_string())),
                ("SUB".to_string(), Some("-".to_string())),
                ("ADD".to_string(), Some("+".to_string())),
                ("SEMI".to_string(), Some(";".to_string()))
            ]);
            symbol_table.extend_nonterminals(["A1".to_string(), "X".to_string(), "A".to_string(), "A2".to_string()]);
            prules.extend([
                prule!(t 1, nt 0; e),    // A1 -> - A1 | ε
                prule!(t 0, nt 2),       // X -> > A         <-- start I
                prule!(nt 0, nt 3, t 3), // A -> A1 A2 ;     <-- start II
                prule!(t 2, nt 3; e),    // A2 -> + A2 | ε
            ]);
            start = Some(1);
        }
        8 => {
            // ambiguous
            prules.extend([
                prule!(nt 0, t 0, nt 0; t 1),    // A -> A a A | b
            ]);
        }
        9 => {
            // simple rules
            prules.extend([
                prule!(t 0, nt 1, t 2),          // A -> a B c
                prule!(t 1; t 3),                // B -> b | d
            ])
        }
        14 => {
            // A -> A A | a
            prules.extend([
                prule!(nt 0, nt 0; t 0)
            ]);
        }
        16 => {
            // A -> B A | b
            // B -> a
            prules.extend([
                prule!(nt 1, nt 0; t 1),
                prule!(t 0)
            ]);
        }
        17 => { // circular dependency (works as long as there's a non-terminal in the loop and an accepting alternative)
            // A -> B | a
            // B -> C ')'
            // C -> '(' A
            symbol_table.extend_terminals([
                ("a".to_string(), Some("a".to_string())),
                ("(".to_string(), Some("(".to_string())),
                (")".to_string(), Some(")".to_string())),
            ]);
            prules.extend([
                prule!(nt 1; t 0),
                prule!(nt 2, t 2),
                prule!(t 1, nt 0),
            ]);
        }
        18 => {
            // A -> a
            prules.extend([
                prule!(t 0),
            ]);
        }
        19 => {
            // A -> a | ε
            prules.extend([
                prule!(t 0; e),
            ]);
        }
        20 => {
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> id ':' id ';' LIST | '}'
            symbol_table.extend_terminals([
                /* 0 */ ("struct".to_string(), Some("struct".to_string())),
                /* 1 */ ("{".to_string(), Some("{".to_string())),
                /* 2 */ ("}".to_string(), Some("}".to_string())),
                /* 3 */ (":".to_string(), Some(":".to_string())),
                /* 4 */ (";".to_string(), Some(";".to_string())),
                /* 5 */ ("id".to_string(), None),
            ]);
            symbol_table.extend_nonterminals([
                /* 0 */ "STRUCT".to_string(),
                /* 1 */ "LIST".to_string(),
            ]);
            prules.extend([
                prule!(t 0, t 5, t 1, nt 1),
                prule!(t 5, t 3, t 5, t 4, nt 1; t 2),
            ]);
        }
        24 => {
            // A -> a (b c)+ d | e
            // =>
            // A -> a B d | e
            // B -> b c B | b c
            prules.extend([
                prule!(t 0, nt 1, t 3; t 4),
                prule!(t 1, t 2, nt 1; t 1, t 2),
            ]);
        }
        25 => {
            // A -> A a b c | A a b d | A a e | f
            prules.extend([
                prule!(nt 0, t 0, t 1, t 2; nt 0, t 0, t 1, t 3; nt 0, t 0, t 4; t 5)
            ]);
        }
        27 => {
            // A -> A a | A b | c | d
            prules.extend([
                prule!(nt 0, t 0; nt 0, t 1; t 2; t 3),
            ]);
        }
        28 => {
            // A -> a | a b | a b c | a b d | e
            prules.extend([
                prule!(t 0; t 0, t 1; t 0, t 1, t 2; t 0, t 1, t 3; t 4),
            ]);
        }
        29 => { // L-form counterpart of #16
            // A -> <L> B A | b
            // B -> a
            prules.extend([
                prule!(nt 1, nt 0; t 1),
                prule!(t 0)
            ]);
            rules.set_flags(0, ruleflag::L_FORM);
        }
        30 => { // L-form counterpart of #20
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> <L> id ':' id ';' LIST | '}'
            symbol_table.extend_terminals([
                /* 0 */ ("struct".to_string(), Some("struct".to_string())),
                /* 1 */ ("{".to_string(), Some("{".to_string())),
                /* 2 */ ("}".to_string(), Some("}".to_string())),
                /* 3 */ (":".to_string(), Some(":".to_string())),
                /* 4 */ (";".to_string(), Some(";".to_string())),
                /* 5 */ ("id".to_string(), None),
            ]);
            symbol_table.extend_nonterminals([
                /* 0 */ "STRUCT".to_string(),
                /* 1 */ "LIST".to_string(),
            ]);
            prules.extend([
                prule!(t 0, t 5, t 1, nt 1),
                prule!(t 5, t 3, t 5, t 4, nt 1; t 2),
            ]);
            rules.set_flags(1, ruleflag::L_FORM);
        }
        31 => {
            // E -> F | E . id
            // F -> id
            prules.extend([
                prule!(nt 1; nt 0, t 0, t 1),
                prule!(t 1),
            ]);
            symbol_table.extend_nonterminals(["E".to_string(), "F".to_string()]);
            symbol_table.extend_terminals([
                (".".to_string(), Some(".".to_string())),
                ("id".to_string(), None),
            ]);
        }
        32 => {
            // E -> F | E . id | E . id ( )
            // F -> id
            prules.extend([
                prule!(nt 1; nt 0, t 0, t 1; nt 0, t 0, t 1, t 2, t 3),
                prule!(t 1),
            ]);
            symbol_table.extend_nonterminals(["E".to_string(), "F".to_string()]);
            symbol_table.extend_terminals([
                (".".to_string(), Some(".".to_string())),
                ("id".to_string(), None),
                ("(".to_string(), Some("(".to_string())),
                (")".to_string(), Some(")".to_string())),
            ]);
        }
        33 => {
            // A -> A a | b c | b d
            prules.extend([
                prule!(nt 0, t 0; t 1, t 2; t 1, t 3),
            ]);
        }
        34 => {
            // S -> id = VAL | exit | return VAL
            // VAL -> id | num
            prules.extend([
                prule!(t 0, t 2, nt 1; t 3; t 4, nt 1),
                prule!(t 0; t 1),
            ]);
            symbol_table.extend_nonterminals(["S".to_string(), "VAL".to_string()]);
            symbol_table.extend_terminals([
                ("id".to_string(), None),
                ("num".to_string(), None),
                ("=".to_string(), Some("=".to_string())),
                ("exit".to_string(), Some("exit".to_string())),
                ("return".to_string(), Some("return".to_string())),
            ]);
        }
        35 => {
            // A -> a | a b b | a c c
            prules.extend([
                prule!(t 0; t 0, t 1, t 1; t 0, t 2, t 2),
            ]);
        }
        36 => {
            // E -> F | num | E . id
            // F -> id
            prules.extend([
                prule!(nt 1; t 2; nt 0, t 0, t 1),
                prule!(t 1),
            ]);
            symbol_table.extend_nonterminals(["E".to_string(), "F".to_string()]);
            symbol_table.extend_terminals([
                (".".to_string(), Some(".".to_string())),
                ("id".to_string(), None),
                ("num".to_string(), None),
            ]);
        }
        37 => {
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> id ':' id ';' LIST | id ';' LIST | '}'
            symbol_table.extend_terminals([
                /* 0 */ ("struct".to_string(), Some("struct".to_string())),
                /* 1 */ ("{".to_string(), Some("{".to_string())),
                /* 2 */ ("}".to_string(), Some("}".to_string())),
                /* 3 */ (":".to_string(), Some(":".to_string())),
                /* 4 */ (";".to_string(), Some(";".to_string())),
                /* 5 */ ("id".to_string(), None),
            ]);
            symbol_table.extend_nonterminals([
                /* 0 */ "STRUCT".to_string(),
                /* 1 */ "LIST".to_string(),
            ]);
            prules.extend([
                prule!(t 0, t 5, t 1, nt 1),
                prule!(t 5, t 3, t 5, t 4, nt 1; t 5, t 4, nt 1 ; t 2),
            ]);
        }
        38 => {
            // A -> A a | A b | b c | b d
            prules.extend([
                prule!(nt 0, t 0; nt 0, t 1; t 1, t 2; t 1, t 3),
            ]);
        }
        39 => {
            // A -> A a b | A a c | b c | b d
            prules.extend([
                prule!(nt 0, t 0, t 1; nt 0, t 0, t 2; t 1, t 2; t 1, t 3),
            ]);
        }
        40 => {
            // A -> a A | b
            prules.extend([
                prule!(t 0, nt 0; t 1),
            ]);
        }
        41 => {
            // A -> a A <L> | b (by explicitly setting the l-form flag)
            prules.extend([
                prule!(t 0, nt 0; t 1),
            ]);
            rules.set_flags(0, ruleflag::L_FORM);
        }
        42 => {
            // A -> a A <L> | b (l-form set through the Alternative flags)
            prules.extend([
                prule!(#L, t 0, nt 0; t 1),
            ]);
        }
        43 => {
            // BATCH -> GROUP ';' BATCH <L> | ε
            // GROUP -> '[' EXPR ']' | '(' EXPR ')'
            // EXPR -> FACTOR '*' FACTOR;
            // FACTOR -> id | int | '(' EXPR ')';
            symbol_table.extend_terminals([
                ("[".to_string(), Some("[".to_string())),   // 0
                ("]".to_string(), Some("]".to_string())),   // 1
                ("(".to_string(), Some("(".to_string())),   // 2
                (")".to_string(), Some(")".to_string())),   // 3
                ("*".to_string(), Some("*".to_string())),   // 4
                ("id".to_string(), None),                   // 5
                ("int".to_string(), None),                  // 6
                (";".to_string(), Some(";".to_string())),   // 7
            ]);
            symbol_table.extend_nonterminals([
                "BATCH".to_string(),                        // 0
                "GROUP".to_string(),                        // 1
                "EXPR".to_string(),                         // 2
                "FACTOR".to_string(),                       // 3
            ]);
            prules.extend([
                prule!(#L, nt 1, t 7, nt 0; e),
                prule!(t 0, nt 2, t 1; t 2, nt 2, t 3),
                prule!(nt 3, t 4, nt 3),
                prule!(t 5; t 6; t 2, nt 2, t 3),
            ]);
        }

        // fixes ---------------------------------------------------------------
        44 => {
            // PRS(63) generated strange code for the rrec E5 -> E6 ^ E5 | E6:
            // we compare here rrec and rrec + lfact, with another NT that has value

            // A -> B a A | B
            // B -> b
            prules.extend([
                prule!(nt 1, t 0, nt 0; nt 1),
                prule!(t 1),
            ])
        }
        45 => {
            // A -> a A | B
            // B -> b
            prules.extend([
                prule!(t 0, nt 0; nt 1),
                prule!(t 1),
            ])
        }
        46 => {
            // A -> A B a | B
            // B -> b
            prules.extend([
                prule!(nt 0, nt 1, t 0; nt 1),
                prule!(t 1),
            ])
        }
        47 => {
            // same as 44 with <L>
            // A -> B a A <L> | B
            // B -> b
            prules.extend([
                prule!(#L, nt 1, t 0, nt 0; nt 1),
                prule!(t 1),
            ])
        }
        48 => {
            // A -> a A <L> | B
            // B -> b
            prules.extend([
                prule!(#L, t 0, nt 0; nt 1),
                prule!(t 1),
            ])
        }

        // ambiguous grammar reconstruction tests ------------------------------
        50 => {
            // classical ambiguous arithmetic grammar
            // E -> E '^' E | E '*' E | E '+' E | F;
            // F -> ( E ) | NUM | ID
            symbol_table.extend_terminals([
                ("ABS".to_string(), Some("abs".to_string())),   // 0
                ("NEG".to_string(), Some("-".to_string())),     // 1
                ("EXP".to_string(), Some("^".to_string())),     // 2
                ("MUL".to_string(), Some("*".to_string())),     // 3
                ("ADD".to_string(), Some("+".to_string())),     // 4
                ("LPAREN".to_string(), Some("(".to_string())),  // 5
                ("RPAREN".to_string(), Some(")".to_string())),  // 6
                ("NUM".to_string(), None),                      // 7
                ("ID".to_string(), None)                        // 8
            ]);
            symbol_table.extend_nonterminals(["E".to_string()]);   // 0
            symbol_table.extend_nonterminals(["F".to_string()]);   // 1
            prules.extend([
                prule!(nt 0, t 2, nt 0; nt 0, t 3, nt 0; nt 0, t 4, nt 0; nt 1),
                prule!(t 5, nt 0, t 6; t 7; t 8),
            ]);
        }
        51 => {
            // classical ambiguous arithmetic grammar
            // E -> 'abs' E | E '^' E | E '\'' | E '*' E | '-' E | E '+' E | F;
            // F -> ( E ) | NUM | ID
            symbol_table.extend_terminals([
                ("ABS".to_string(), Some("abs".to_string())),   // 0
                ("NEG".to_string(), Some("-".to_string())),     // 1
                ("EXP".to_string(), Some("^".to_string())),     // 2
                ("MUL".to_string(), Some("*".to_string())),     // 3
                ("ADD".to_string(), Some("+".to_string())),     // 4
                ("LPAREN".to_string(), Some("(".to_string())),  // 5
                ("RPAREN".to_string(), Some(")".to_string())),  // 6
                ("NUM".to_string(), None),                      // 7
                ("ID".to_string(), None),                       // 8
                ("PRIME".to_string(), Some("'".to_string())),   // 9
            ]);
            symbol_table.extend_nonterminals(["E".to_string()]);   // 0
            symbol_table.extend_nonterminals(["F".to_string()]);   // 1
            prules.extend([
                prule!(t 0, nt 0;
                    nt 0, t 2, nt 0;
                    nt 0, t 9;
                    nt 0, t 3, nt 0;
                    t 1, nt 0;
                    nt 0, t 4, nt 0;
                    nt 1),
                prule!(t 5, nt 0, t 6; t 7; t 8),
            ]);
        }
        52 => {
            // lrec chain:
            // E -> E * E | E ! | E ' | E + E | F;
            // F -> NUM | ID
            symbol_table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),     // 0
                ("FAC".to_string(), Some("!".to_string())),     // 1
                ("PRIME".to_string(), Some("'".to_string())),   // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("NUM".to_string(), None),                      // 4
                ("ID".to_string(), None),                       // 5
            ]);
            symbol_table.extend_nonterminals(["E".to_string()]);   // 0
            symbol_table.extend_nonterminals(["F".to_string()]);   // 1
            prules.extend([
                prule!(nt 0, t 0, nt 0; nt 0, t 1; nt 0, t 2; nt 0, t 3, nt 0; nt 1),
                prule!(t 4; t 5),
            ]);
        }
        53 => {
            // E -> E ^ E <R> | E * E <R> | - E | E + E | F
            // F ->  ID | NUM | ( E )
            symbol_table.extend_terminals([
                ("EXP".to_string(), Some("^".to_string())),     // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("NEG".to_string(), Some("-".to_string())),     // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
                ("NUM".to_string(), None),                      // 5
                ("LPAREN".to_string(), Some("(".to_string())),  // 6
                ("RPAREN".to_string(), Some(")".to_string())),  // 7
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
                "F".to_string(),        // 1
            ]);
            prules.extend([
                prule!(#R, nt 0, t 0, nt 0; #R, nt 0, t 1, nt 0; t 2, nt 0; nt 0, t 3, nt 0; nt 1),
                prule!(t 4; t 5; t 6, nt 0, t 7),
            ]);
        }
        54 => {
            // E -> E * E <R> | E ! | E -- | E + E <R> | ID | NUM
            symbol_table.extend_terminals([
                ("FACT".to_string(), Some("!".to_string())),    // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("DEC".to_string(), Some("--".to_string())),    // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
                ("NUM".to_string(), None),                      // 5
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(#R, nt 0, t 1, nt 0; nt 0, t 0; nt 0, t 2; #R, nt 0, t 3, nt 0; t 4; t 5)
            ]);
        }
        55 => {
            // E -> E * E | E -- | ! E | E + E | ID | NUM
            symbol_table.extend_terminals([
                ("FACT".to_string(), Some("!".to_string())),    // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("DEC".to_string(), Some("--".to_string())),    // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
                ("NUM".to_string(), None),                      // 5
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 1, nt 0; nt 0, t 2; t 0, nt 0; nt 0, t 3, nt 0; t 4; t 5)
            ]);
        }
        56 => {
            // E -> E * E | ! E | E -- | E + E | ID | NUM
            symbol_table.extend_terminals([
                ("FACT".to_string(), Some("!".to_string())),    // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("DEC".to_string(), Some("--".to_string())),    // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
                ("NUM".to_string(), None),                      // 5
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 1, nt 0; t 0, nt 0; nt 0, t 2; nt 0, t 3, nt 0; t 4; t 5)
            ]);
        }
        57 => {
            // E -> E ^ E | E * E | E + E | ID | NUM
            symbol_table.extend_terminals([
                ("EXP".to_string(), Some("^".to_string())),     // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("ADD".to_string(), Some("+".to_string())),     // 2
                ("ID".to_string(), None),                       // 3
                ("NUM".to_string(), None),                      // 4
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 0, nt 0; nt 0, t 1, nt 0; nt 0, t 2, nt 0; t 3; t 4)
            ]);
        }
        58 => {
            // E -> E + | - E | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 0; t 1, nt 0; t 2)
            ])
        }
        59 => {
            // E -> E + E | - E | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 0, nt 0; t 1, nt 0; t 2)
            ])
        }
        60 => {
            // E -> E + | <L> - E | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 0; #L, t 1, nt 0; t 2)
            ])
        }
        61 => {
            // compare to 58
            // E -> E + | - E | 0 | 1
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
                ("ONE".to_string(), Some("1".to_string())),     // 3
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 0; t 1, nt 0; t 2; t 3)
            ])
        }
        62 => {
            // E -> E * E | - E | E + E | ID;
            symbol_table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),     // 0
                ("NEG".to_string(), Some("-".to_string())),     // 1
                ("ADD".to_string(), Some("+".to_string())),     // 2
                ("ID".to_string(), None),                       // 3
            ]);
            symbol_table.extend_nonterminals(["E".to_string()]);   // 0
            prules.extend([
                prule!(nt 0, t 0, nt 0; t 1, nt 0; nt 0, t 2, nt 0; t 3),
            ]);
        }
        63 => {
            // E -> <R> E ^ E | E * E | - E | E + E | ID;
            symbol_table.extend_terminals([
                ("EXP".to_string(), Some("^".to_string())),     // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("NEG".to_string(), Some("-".to_string())),     // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
                // "E3".to_string(),       // 1
                // "E5".to_string(),       // 2
                // "E6".to_string(),       // 3
            ]);
            prules.extend([
                prule!(#R, nt 0, t 0, nt 0; nt 0, t 1, nt 0; t 2, nt 0; nt 0, t 3, nt 0; t 4),
            ]);
        }
        64 => {
            // E -> - E | E + E | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(t 1, nt 0; nt 0, t 0, nt 0; t 2)
            ])
        }
        65 => {
            // E -> E ! | E * E | E + | - E | ID
            symbol_table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),     // 0
                ("FACT".to_string(), Some("!".to_string())),    // 1
                ("ADD".to_string(), Some("+".to_string())),     // 2
                ("SUB".to_string(), Some("-".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 1; nt 0, t 0, nt 0; nt 0, t 2; t 3, nt 0; t 4),
            ])
        }
        66 => {
            // left factorization issues:
            // E -> E . * E | E -- | E . + E | ! E | ID
            symbol_table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),     // 0
                ("DEC".to_string(), Some("--".to_string())),    // 1
                ("ADD".to_string(), Some("+".to_string())),     // 2
                ("NOT".to_string(), Some("!".to_string())),     // 3
                ("DOT".to_string(), Some(".".to_string())),      // 4
                ("ID".to_string(), None),                       // 5
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(nt 0, t 4, t 0, nt 0; nt 0, t 1; nt 0, t 4, t 2, nt 0; t 3, nt 0; t 5)
            ]);
        }

        // test of mixed recursions --------------------------------------------
        70 => {
            // E -> - E | E + | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prules.extend([
                prule!(t 1, nt 0; nt 0, t 0; t 2)
            ])
        }

        // ambiguity? ----------------------------------------------------------
        100 => {
            // A -> A a A b | c (amb removed)
            prules.extend([
                prule!(nt 0, t 0, nt 0, t 1; t 2),
            ]);
        }
        101 => {
            // A -> a A A | b (no amb)
            prules.extend([
                prule!(t 0, nt 0, nt 0; t 1),
            ]);
        }
        102 => {
            // A -> A a A b A | c (amb removed)
            prules.extend([
                prule!(nt 0, t 0, nt 0, t 1, nt 0; t 2)
            ]);
        }
        103 => {
            // A -> a (A b A)* c | d (no amb)
            // =>
            // A -> a B c | d
            // B -> A b A B | ε
            prules.extend([
                prule!(t 0, nt 1, t 2; t 3),
                prule!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        104 => {
            // A -> (A b A)* c | a (amb A:a, B:c)
            // =>
            // A -> B c | a
            // B -> A b A B | ε
            prules.extend([
                prule!(nt 1, t 2; t 0),
                prule!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        105 => {
            // A -> a (A b A)* | c (amb)
            // =>
            // A -> a B | c
            // B -> A b A B | ε
            prules.extend([
                prule!(t 0, nt 1; t 2),
                prule!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        106 => {
            // A -> (A b A)* | a (very amb)
            // =>
            // A -> A1 | a
            // A1 -> A b A A1 | ε
            prules.extend([
                prule!(nt 1; t 0),
                prule!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }

        // warnings and errors -------------------------------------------------
        1000 => { // A -> A a  (error: missing non-recursive alternative)
            prules.extend([
                prule!(nt 0, t 0)
            ]);
        },
        1001 => { // A -> A a A A | b (error: cannot remove recursion)
            prules.extend([
                prule!(nt 0, t 0, nt 0, nt 0; t 1)
            ]);
        },
        1002 => { // A -> A a A a A | b (warning: ambiguous)
            prules.extend([
                prule!(nt 0, t 0, nt 0, t 0, nt 0; t 1)
            ]);
        },
        1003 => { // (error: no terminal in grammar)
            prules.extend([
                prule!(nt 1),
                prule!(nt 2),
                prule!(nt 0),
            ]);
        },
        1004 => { // (error: no terminal used in table)
            prules.extend([
                prule!(nt 1),
                prule!(nt 2, t 0),
                prule!(nt 0),
            ]);
        },
        1005 => { // (warnings: unused terminals, unused nonterminals)
            symbol_table.extend_terminals([("a".to_string(), None), ("b".to_string(), None)]);
            symbol_table.extend_nonterminals(["A".to_string(), "B".to_string()]);
            prules.extend([
                prule!(t 1),
                prule!(t 1),
            ]);
        },
        1006 => { // symbol_table.num_nt != rules.num_nt
            extend_nt = false;
            symbol_table.extend_terminals([("a".to_string(), None), ("b".to_string(), None)]);
            symbol_table.extend_nonterminals(["A".to_string()]);
            prules.extend([
                prule!(t 0, nt 1),
                prule!(t 1),
            ])
        }
        _ => {}
    };
    for (v, f) in flags {
        rules.set_flags(v, f);
    }
    for (child, parent) in parents {
        rules.set_parent(child, parent);
    }
    rules.calc_num_symbols();
    let calc_num_nt = if extend_nt { rules.num_nt } else { symbol_table.get_num_nt() };
    complete_symbol_table(&mut symbol_table, rules.num_t, calc_num_nt, is_t_data);
    rules.set_symbol_table(symbol_table);
    if let Some(start) = start {
        rules.set_start(start);
    }
    rules
}

// ---------------------------------------------------------------------------------------------

#[test]
fn prs_remove_recursion() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
            // A -> A b | A c | d | d e     A -> d A_1 | d e A_1
            // B -> A f | g | h             B -> A f | g | h
            //                              A_1 -> b A_1 | c A_1 | ε
            0 => prule!(t 3, nt 2; t 3, t 4, nt 2),
            1 => prule!(nt 0, t 5; t 6; t 7),
            2 => prule!(t 1, nt 2; t 2, nt 2; e),
        ]),
        (2, btreemap![]),
        (4, btreemap![
            // E -> E - T | E + T | T     E -> T E_1
            // T -> T / F | T * F | F     T -> F T_1
            // F -> ( E ) | N | I         F -> ( E ) | NUM | ID
            //                            E_1 -> + T E_1 | - T E_1 | ε
            //                            T_1 -> * F T_1 | / F T_1 | ε
            0 => prule!(nt 1, nt 3),
            1 => prule!(nt 2, nt 4),
            2 => prule!(t 4, nt 0, t 5; t 6; t 7),
            3 => prule!(t 0, nt 1, nt 3; t 1, nt 1, nt 3; e),
            4 => prule!(t 2, nt 2, nt 4; t 3, nt 2, nt 4; e),
        ]),
        (8, btreemap![
            // (0) A -> A_2 A_1
            // (1) A_1 -> a A_2 A_1 | ε
            // (2) A_2 -> b
            0 => prule!(nt 2, nt 1),
            1 => prule!(t 0, nt 2, nt 1; e),
            2 => prule!(t 1),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let mut rules = build_prs(test_id, false);
        if VERBOSE {
            println!("{:=<80}\ntest {test_id}:", "");
            rules.print_rules(false, false);
        }
        rules.remove_recursion();
        let result = <BTreeMap<_, _>>::from(&rules);
        if VERBOSE {
            println!("=>");
            rules.print_rules(true, false);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(rules.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(rules.log.get_warnings().join("\n"), "", "test {test_id} failed");
        rules.remove_recursion();
        let result = <BTreeMap<_, _>>::from(&rules);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
    }
}

#[test]
fn prs_left_factorize() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
            // A -> d A_1 | A A_2
            // B -> A f | g | h
            // A_1 -> e | ε
            // A_2 -> b | c
            0 => prule!(t 3, nt 2; nt 0, nt 3),
            1 => prule!(nt 0, t 5; t 6; t 7),
            2 => prule!(t 4; e),
            3 => prule!(t 1; t 2),
        ]),
        (1, btreemap![
            // A -> b A_1 | c b | d c A_2
            // A_1 -> b c A_3 | c g | d e A_4
            // A_2 -> e | ε
            // A_3 -> d | ε
            // A_4 -> f | g
            0 => prule!(t 1, nt 1; t 2, t 1; t 3, t 2, nt 2),
            1 => prule!(t 1, t 2, nt 3; t 2, t 6; t 3, t 4, nt 4),
            2 => prule!(t 4; e),
            3 => prule!(t 3; #R, e),
            4 => prule!(t 5; t 6),
        ]),
        (2, btreemap![]),
        (3, btreemap![
            // A -> a
            // B -> c B_1
            // C -> c | b
            // D -> d
            // B_1 -> a | ε
            0 => prule!(t 0),
            1 => prule!(t 2, nt 4),
            2 => prule!(t 2; t 1),
            3 => prule!(t 3),
            4 => prule!(t 0; e),
        ]),
        (4, btreemap![
            // E -> E E_1 | T
            // T -> T T_1 | F
            // F -> ( E ) | NUM | ID
            // E_1 -> + T | - T
            // T_1 -> * F | / F
            0 => prule!(nt 0, nt 3; nt 1),
            1 => prule!(nt 1, nt 4; nt 2),
            2 => prule!(t 4, nt 0, t 5; t 6; t 7),
            3 => prule!(t 0, nt 1; t 1, nt 1),
            4 => prule!(t 2, nt 2; t 3, nt 2),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let mut rules = build_prs(test_id, false);
        if VERBOSE {
            println!("test {test_id}:");
            rules.print_rules(false, false);
        }
        rules.left_factorize();
        let result = BTreeMap::<_, _>::from(&rules);
        if VERBOSE {
            println!("=>");
            rules.print_rules(true, false);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(rules.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(rules.log.get_warnings().join("\n"), "", "test {test_id} failed");
        rules.left_factorize();
        let result = BTreeMap::<_, _>::from(&rules);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
    }
}

#[test]
fn prs_ll1_from() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
            // A -> d A_2
            // B -> A f | g | h
            // A_1 -> b A_1 | c A_1 | ε
            // A_2 -> e A_1 | A_1
            0 => prule!(t 3, nt 3),
            1 => prule!(nt 0, t 5; t 6; t 7),
            2 => prule!(t 1, nt 2; t 2, nt 2; e),
            3 => prule!(t 4, nt 2; nt 2),
        ]),
        (1, btreemap![
            // A -> b A_1 | c b | d c A_2
            // A_1 -> b c A_3 | c g | d e A_4
            // A_2 -> e | ε
            // A_3 -> d | ε
            // A_4 -> f | g
            0 => prule!(t 1, nt 1; t 2, t 1; t 3, t 2, nt 2),
            1 => prule!(t 1, t 2, nt 3; t 2, t 6; t 3, t 4, nt 4),
            2 => prule!(t 4; e),
            3 => prule!(t 3; #256, e),
            4 => prule!(t 5; t 6),
        ]),
        (4, btreemap![
            // E -> T E_1
            // T -> F T_1
            // F -> ( E ) | NUM | ID
            // E_1 -> + T E_1 | - T E_1 | ε
            // T_1 -> * F T_1 | / F T_1 | ε
            0 => prule!(nt 1, nt 3),
            1 => prule!(nt 2, nt 4),
            2 => prule!(t 4, nt 0, t 5; t 6; t 7),
            3 => prule!(t 0, nt 1, nt 3; t 1, nt 1, nt 3; e),
            4 => prule!(t 2, nt 2, nt 4; t 3, nt 2, nt 4; e),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let rules_lr = build_prs(test_id, false);
        if VERBOSE {
            println!("test {test_id}:");
            rules_lr.print_rules(false, false);
        }
        let ll1 = ProdRuleSet::<LL1>::build_from(rules_lr.clone());
        let result = BTreeMap::<_, _>::from(&ll1);
        if VERBOSE {
            println!("=>");
            ll1.print_rules(true, false);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
        let rules_ll1 = ProdRuleSet::<LL1>::build_from(rules_lr.clone());
        let result = BTreeMap::<_, _>::from(&rules_ll1);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
   }
}

#[test]
#[should_panic]
/// We test that the code compiles, but it must also panic because the `remove_ambiguity()`
/// method isn't implemented yet for LR grammars.
fn prs_lr_from() {
    let mut test_id = 0;
    loop {
        let rules = build_prs(test_id, false);
        if rules.prules.is_empty() {
            break;
        }
        let _lr = ProdRuleSet::<LR>::build_from(rules);
        test_id += 1;
    }
}

#[test]
fn prs_calc_first() {
    let tests: Vec<(u32, VarId, HashMap<Symbol, HashSet<Symbol>>)> = vec![
        (4, 0, hashmap![
            // E -> E + T | E - T | T
            // T -> T * F | T / F | F
            // F -> ( E ) | NUM | ID
            // ->
            // E -> T E_1
            // T -> F T_1
            // F -> ( E ) | NUM | ID
            // E_1 -> + T E_1 | - T E_1 | ε
            // T_1 -> * F T_1 | / F T_1 | ε
            //
            // T:  0:+, 1:-, 2:*, 3:/, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F, 3:E_1, 4:T_1
            sym!(e) => hashset![sym!(e)],
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(t 3) => hashset![sym!(t 3)],
            sym!(t 4) => hashset![sym!(t 4)],
            sym!(t 5) => hashset![sym!(t 5)],
            sym!(t 6) => hashset![sym!(t 6)],
            sym!(t 7) => hashset![sym!(t 7)],
            sym!(nt 0) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 1) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 2) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 3) => hashset![sym!(t 0), sym!(t 1), sym!(e)],
            sym!(nt 4) => hashset![sym!(t 2), sym!(t 3), sym!(e)],
        ]),
        (5, 0, hashmap![
            sym!(e) => hashset![sym!(e)],
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(nt 0) => hashset![sym!(t 0), sym!(t 1), sym!(t 2)],
            sym!(nt 1) => hashset![sym!(t 0), sym!(e)],
            sym!(nt 2) => hashset![sym!(t 1), sym!(e)]
        ]),
        (43, 0, hashmap![
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(t 3) => hashset![sym!(t 3)],
            sym!(t 4) => hashset![sym!(t 4)],
            sym!(t 5) => hashset![sym!(t 5)],
            sym!(t 6) => hashset![sym!(t 6)],
            sym!(t 7) => hashset![sym!(t 7)],
            sym!(nt 0) => hashset![sym!(t 0), sym!(t 2), sym!(e)],
            sym!(nt 1) => hashset![sym!(t 0), sym!(t 2)],
            sym!(nt 2) => hashset![sym!(t 2), sym!(t 5), sym!(t 6)],
            sym!(nt 3) => hashset![sym!(t 2), sym!(t 5), sym!(t 6)],
            sym!(e) => hashset![sym!(e)],
        ]),
        (51, 0, hashmap![
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(t 3) => hashset![sym!(t 3)],
            sym!(t 4) => hashset![sym!(t 4)],
            sym!(t 5) => hashset![sym!(t 5)],
            sym!(t 6) => hashset![sym!(t 6)],
            sym!(t 7) => hashset![sym!(t 7)],
            sym!(t 8) => hashset![sym!(t 8)],
            sym!(t 9) => hashset![sym!(t 9)],
            sym!(nt 0) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(nt 1) => hashset![sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(nt 2) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 9), sym!(e)],
            sym!(nt 3) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(nt 4) => hashset![sym!(t 2), sym!(t 3), sym!(t 9), sym!(e)],
            sym!(nt 5) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(nt 6) => hashset![sym!(t 2), sym!(t 9), sym!(e)],
            sym!(nt 7) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(e) => hashset![sym!(e)],
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, start, expected) in tests {
        let rules_lr = build_prs(test_id, false);
        if VERBOSE {
            println!("test {test_id}:");
        }
        let mut ll1 = ProdRuleSet::<LL1>::build_from(rules_lr.clone());
        ll1.set_start(start);
        let first = ll1.calc_first();
        if VERBOSE {
            ll1.print_rules(false, false);
            let b = map_and_print_first(&first, ll1.get_symbol_table());
            for (sym, set) in &b {
                println!("            sym!({}) => hashset![{}],", sym.to_macro_item(),
                         set.iter().map(|s| format!("sym!({})", s.to_macro_item())).join(", "));
            }
        }
        assert_eq!(first, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
   }
}

#[test]
fn prs_calc_follow() {
    let tests: Vec<(u32, VarId, HashMap<Symbol, HashSet<Symbol>>)> = vec![
        (4, 0, hashmap![
            // E -> T E_1
            // T -> F T_1
            // F -> ( E ) | NUM | ID
            // E_1 -> + T E_1 | - T E_1 | ε
            // T_1 -> * F T_1 | / F T_1 | ε
            //
            // T:  0:+, 1:-, 2:*, 3:/, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F, 3:E_1, 4:T_1
            sym!(nt 0) => hashset![sym!(t 5), sym!(end)],
            sym!(nt 1) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(end)],
            sym!(nt 2) => hashset![sym!(t 0), sym!(t 1), sym!(t 2), sym!(t 3), sym!(t 5), sym!(end)],
            sym!(nt 3) => hashset![sym!(t 5), sym!(end)],
            sym!(nt 4) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(end)],
        ]),
        (5, 0, hashmap![
            sym!(nt 0) => hashset![sym!(end)],
            sym!(nt 1) => hashset![sym!(t 1), sym!(t 2)],
            sym!(nt 2) => hashset![sym!(t 2)],
        ]),
        (43, 0, hashmap![
            sym!(nt 0) => hashset![sym!(end)],
            sym!(nt 1) => hashset![sym!(t 7)],
            sym!(nt 2) => hashset![sym!(t 1), sym!(t 3)],
            sym!(nt 3) => hashset![sym!(t 1), sym!(t 3), sym!(t 4)],
        ]),
        (51, 0, hashmap![
            sym!(nt 0) => hashset![sym!(t 6), sym!(end)],
            sym!(nt 1) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 2) => hashset![sym!(t 6), sym!(end)],
            sym!(nt 3) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 4) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 5) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 6) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 7) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, start, expected) in tests {
        let rules_lr = build_prs(test_id, false);
        if VERBOSE {
            println!("test {test_id}:");
        }
        let mut ll1 = ProdRuleSet::<LL1>::build_from(rules_lr.clone());
        ll1.set_start(start);
        let first = ll1.calc_first();
        let follow = ll1.calc_follow(&first);
        if VERBOSE {
            ll1.print_rules(false, false);
            let b = map_and_print_follow(&follow, ll1.get_symbol_table());
            for (sym, set) in &b {
                println!("            sym!({}) => hashset![{}],", sym.to_macro_item(),
                         set.iter().map(|s| format!("sym!({})", s.to_macro_item())).join(", "));
            }
        }
        assert_eq!(follow, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
   }
}

#[test]
fn prs_grammar_notes() {
    let tests: Vec<(T, VarId, Vec<&str>, Vec<&str>)> = vec![
        //        warnings                                  errors
        //        -------------------------------------     -------------------------------------
        (T::PRS(1000), 0, vec![],                           vec!["recursive rules must have at least one independent alternative"]),
        (T::PRS(1002), 0, vec!["ambiguity for NT"],         vec![]),
        (T::PRS(1003), 0, vec![],                           vec!["no terminal in grammar"]),
        (T::PRS(1004), 0, vec![],                           vec!["no terminal used in the table"]),
        (T::PRS(1005), 0, vec!["unused nonterminals",
                               "unused terminals"],         vec![]),
        (T::PRS(1006), 0, vec![],                           vec!["there are 2 rules but the symbol table has 1 nonterminal symbols: dropping the table"]),
        (T::RTS(101), 0,  vec![],                           vec!["there are 2 rules but the symbol table has 1 nonterminal symbols: dropping the table"]),
        (T::RTS(500), 0, vec![],                            vec!["in A, (<L=AIter1> a <L=AIter2>)*: conflicting <L=AIter1> and <L=AIter2>",
                                                                 "normalize_var(AIter2): error while normalizing the rules, 0 remaining nodes instead of 1"]),
        (T::RTS(501), 0, vec![],                            vec!["in A, (<L=AIter1> a | <L=AIter2> b)*: conflicting <L=AIter1> and <L=AIter2>",
                                                                 "normalize_var(AIter2): error while normalizing the rules, 0 remaining nodes instead of 1"]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, start, expected_warnings, expected_errors)) in tests.into_iter().enumerate() {
        if VERBOSE {
            println!("{:=<80}\ntest {test_id} with {ll_id:?}/{start}:", "");
        }
        let mut ll1 = ll_id.try_build_prs(start, false);
        if VERBOSE {
            ll1.print_rules(false, false);
            ll1.print_logs();
        }
        let mut parsing_table = None;
        if ll1.log.num_errors() == 0 {
            let first = ll1.calc_first();
            if ll1.log.num_errors() == 0 {
                let follow = ll1.calc_follow(&first);
                if ll1.log.num_errors() == 0 {
                    parsing_table = Some(ll1.calc_table(&first, &follow, false));
                }
            }
            if VERBOSE {
                println!("=>");
                ll1.print_rules(false, false);
                if let Some(table) = &parsing_table {
                    print_alts(&table.alts, ll1.get_symbol_table());
                    println!("table:");
                    table.print(ll1.get_symbol_table(), 12);
                }
                ll1.print_logs();
            }
        }
        assert_eq!(ll1.log.num_errors(), expected_errors.len(), "test {test_id}/{ll_id:?}/{start} failed on # errors");
        assert_eq!(ll1.log.num_warnings(), expected_warnings.len(), "test {test_id}/{ll_id:?}/{start} failed on # warnings");
        let err_discr = ll1.log.get_errors().zip(expected_errors).filter_map(|(e, ee)|
            if !e.contains(ee) { Some(format!("- \"{e}\" doesn't contain \"{ee}\"")) } else { None }
        ).to_vec();
        assert!(err_discr.is_empty(), "test {test_id}/{ll_id:?}/{start} has discrepancies in the expected error messages:\n{}", err_discr.join("\n"));
        let warn_discr = ll1.log.get_warnings().zip(expected_warnings).filter_map(|(w, ew)|
            if !w.contains(ew) { Some(format!("- \"{w}\" doesn't contain \"{ew}\"")) } else { None }
        ).to_vec();
        assert!(warn_discr.is_empty(), "test {test_id}/{ll_id:?}/{start} has discrepancies in the expected warning messages:\n{}", warn_discr.join("\n"));
   }
}

#[test]
fn prs_calc_table() {
    let tests: Vec<(T, VarId, usize, Vec<(VarId, Alternative)>, Vec<AltId>)> = vec![
        (T::PRS(4), 0, 0, vec![
            // E -> E + T | E - T | T
            // T -> T * F | T / F | F
            // F -> ( E ) | NUM | ID
            // - 0: E -> T E_1
            // - 1: T -> F T_1
            // - 2: F -> ( E )
            // - 3: F -> NUM
            // - 4: F -> ID
            // - 5: E_1 -> + T E_1
            // - 6: E_1 -> - T E_1
            // - 7: E_1 -> ε
            // - 8: T_1 -> * F T_1
            // - 9: T_1 -> / F T_1
            // - 10: T_1 -> ε
            (0, alt!(nt 1, nt 3)),
            (1, alt!(nt 2, nt 4)),
            (2, alt!(t 4, nt 0, t 5)),
            (2, alt!(t 6)),
            (2, alt!(t 7)),
            (3, alt!(t 0, nt 1, nt 3)),
            (3, alt!(t 1, nt 1, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 2, nt 2, nt 4)),
            (4, alt!(t 3, nt 2, nt 4)),
            (4, alt!(e)),
        ], vec![
            //     |  -   +   /   *   (   )   N   I   $
            // ----+-------------------------------------
            // E   |  .   .   .   .   0   p   0   0   p
            // T   |  p   p   .   .   1   p   1   1   p
            // F   |  p   p   p   p   2   p   3   4   p
            // E_1 |  5   6   .   .   .   7   .   .   7
            // T_1 | 10  10   8   9   .  10   .   .  10
             11,  11,  11,  11,   0,  12,   0,   0,  12,
             12,  12,  11,  11,   1,  12,   1,   1,  12,
             12,  12,  12,  12,   2,  12,   3,   4,  12,
              5,   6,  11,  11,  11,   7,  11,  11,   7,
             10,  10,   8,   9,  11,  10,  11,  11,  10,
        ]),
        (T::PRS(5), 0, 0, vec![
            // - 0: A -> A1 A2 ; ;
            // - 1: A1 -> - A1
            // - 2: A1 -> ε
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, alt!(nt 1, nt 2, t 2, t 2)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(e)),
        ], vec![
            //    |  -   +   ;   $
            // ---+-----------------
            // A  |  0   0   0   p
            // A1 |  1   2   2   .
            // A2 |  .   3   4   .
              0,   0,   0,   6,
              1,   2,   2,   5,
              5,   3,   4,   5,
        ]),
        (T::PRS(6), 1, 0, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: A -> A1 A2 ;
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, alt!(t 0, nt 0)),
            (0, alt!(e)),
            (1, alt!(nt 0, nt 2, t 2)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(e)),
        ], vec![
            //    |  -   +   ;   $
            // ---+-----------------
            // A1 |  0   1   1   .
            // A  |  2   2   2   p
            // A2 |  .   3   4   .
              0,   1,   1,   5,
              2,   2,   2,   6,
              5,   3,   4,   5,
        ]),
        (T::PRS(7), 1, 0, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: X -> A
            // - 3: A -> A1 A2 ;
            // - 4: A2 -> + A2
            // - 5: A2 -> ε
            (0, alt!(t 1, nt 0)),
            (0, alt!(e)),
            (1, alt!(t 0, nt 2)),
            (2, alt!(nt 0, nt 3, t 3)),
            (3, alt!(t 2, nt 3)),
            (3, alt!(e)),
        ], vec![
            //    |  >   -   +   ;   $
            // ---+---------------------
            // A1 |  .   0   1   1   .
            // X  |  2   .   .   .   p
            // A  |  .   3   3   3   p
            // A2 |  .   .   4   5   .
              6,   0,   1,   1,   6,
              2,   6,   6,   6,   7,
              6,   3,   3,   3,   7,
              6,   6,   4,   5,   6,
        ]),
        (T::PRS(7), 2, 2, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: A -> A1 A2 ;
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, alt!(t 1, nt 0)),
            (0, alt!(e)),
            (1, alt!(nt 0, nt 2, t 3)),
            (2, alt!(t 2, nt 2)),
            (2, alt!(e)),
        ], vec![
            //    |  >   -   +   ;   $
            // ---+---------------------
            // A1 |  .   0   1   1   .
            // A  |  .   2   2   2   p
            // A2 |  .   .   3   4   .
              5,   0,   1,   1,   5,
              5,   2,   2,   2,   6,
              5,   5,   3,   4,   5,
        ]),
        (T::PRS(8), 0, 0, vec![
            // A -> A a A | b
            // - 0: A -> A_2 A_1
            // - 1: A_1 -> a A_2 A_1
            // - 2: A_1 -> ε
            // - 3: A_2 -> b
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1)),
        ], vec![
            //     |  a   b   $
            // ----+-------------
            // A   |  .   0   p
            // A_1 |  1   .   2
            // A_2 |  p   3   p
              4,   0,   5,
              1,   4,   2,
              5,   3,   5,
        ]),
        (T::PRS(14), 0, 0, vec![
            // - 0: A -> A_2 A_1
            // - 1: A_1 -> A_2 A_1
            // - 2: A_1 -> ε
            // - 3: A_2 -> a
            (0, alt!(nt 2, nt 1)),
            (1, alt!(nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 0)),
        ], vec![
            //     |  a   $
            // ----+---------
            // A   |  0   p
            // A_1 |  1   2
            // A_2 |  3   p
              0,   5,
              1,   2,
              3,   5,
        ]),
        (T::PRS(17), 0, 0, vec![
            // - 0: A -> B
            // - 1: A -> a
            // - 2: B -> C )
            // - 3: C -> ( A
            (0, alt!(nt 1)),
            (0, alt!(t 0)),
            (1, alt!(nt 2, t 2)),
            (2, alt!(t 1, nt 0)),
        ], vec![
            //   |  a   (   )   $
            // --+-----------------
            // A |  1   0   p   p
            // B |  .   2   p   p
            // C |  .   3   p   .
              1,   0,   5,   5,
              4,   2,   5,   5,
              4,   3,   5,   4,
        ]),
        (T::PRS(18), 0, 0, vec![
            // - 0: A -> a
            (0, alt!(t 0)),
        ], vec![
            //   |  a   $
            // --+---------
            // A |  0   p
              0,   2,
        ]),
        (T::PRS(19), 0, 0, vec![
            // - 0: A -> a
            (0, alt!(t 0)),
            (0, alt!(e)),
        ], vec![
            //   |   a   $
            // --+---------
            // A |   0   1
              0,   1,
        ]),
        (T::PRS(20), 0, 0, vec![
            // - 0: STRUCT -> struct id { LIST
            // - 1: LIST -> id : id ; LIST
            // - 2: LIST -> }
            (0, alt!(t 0, t 5, t 1, nt 1)),
            (1, alt!(t 5, t 3, t 5, t 4, nt 1)),
            (1, alt!(t 2)),
        ], vec![
            //        | struct  {   }   :   ;  id   $
            // -------+--------------------------------
            // STRUCT |   0     .   .   .   .   .   p
            // LIST   |   .     .   2   .   .   1   p
              0,   3,   3,   3,   3,   3,   4,
              3,   3,   2,   3,   3,   1,   4,
        ]),
/*
        (22, 0, 0, vec![
        ], vec![
        ]),
        (T::PRS(23), 0, 0, vec![
        ], vec![
        ]),
*/
        (T::PRS(24), 0, 0, vec![
            // - 0: A -> a B d
            // - 1: A -> e
            // - 2: B -> b c B_1
            // - 3: B_1 -> B
            // - 4: B_1 -> ε
            (0, alt!(t 0, nt 1, t 3)),
            (0, alt!(t 4)),
            (1, alt!(t 1, t 2, nt 2)),
            (2, alt!(nt 1)),
            (2, alt!(e)),
        ], vec![
            //     |  a   b   c   d   e   $
            // ----+-------------------------
            // A   |  0   .   .   .   1   p
            // B   |  .   2   .   p   .   .
            // B_1 |  .   3   .   4   .   .
              0,   5,   5,   5,   1,   6,
              5,   2,   5,   6,   5,   5,
              5,   3,   5,   4,   5,   5,
        ]),
        (T::PRS(25), 0, 0, vec![
            // A -> A a b c | A a b d | A a e | f
            // - 0: A -> f A_1
            // - 1: A_1 -> a A_2
            // - 2: A_1 -> ε
            // - 3: A_2 -> b A_3
            // - 4: A_2 -> e A_1
            // - 5: A_3 -> c A_1
            // - 6: A_3 -> d A_1
            (0, alt!(t 5, nt 1)),
            (1, alt!(t 0, nt 2)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 3)),
            (2, alt!(t 4, nt 1)),
            (3, alt!(t 2, nt 1)),
            (3, alt!(t 3, nt 1)),
        ], vec![
            //     |  a   b   c   d   e   f   $
            // ----+-----------------------------
            // A   |  .   .   .   .   .   0   p
            // A_1 |  1   .   .   .   .   .   2
            // A_2 |  .   3   .   .   4   .   p
            // A_3 |  .   .   5   6   .   .   p
              7,   7,   7,   7,   7,   0,   8,
              1,   7,   7,   7,   7,   7,   2,
              7,   3,   7,   7,   4,   7,   8,
              7,   7,   5,   6,   7,   7,   8,
        ]),
        (T::PRS(27), 0, 0, vec![
            // A -> A a | A b | c | d
            // - 0: A -> c A_1
            // - 1: A -> d A_1
            // - 2: A_1 -> a A_1
            // - 3: A_1 -> b A_1
            // - 4: A_1 -> ε
            (0, alt!(t 2, nt 1)),
            (0, alt!(t 3, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(t 1, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  a   b   c   d   $
            // ----+---------------------
            // A   |  .   .   0   1   p
            // A_1 |  2   3   .   .   4
              5,   5,   0,   1,   6,
              2,   3,   5,   5,   4,
        ]),
        (T::PRS(28), 0, 0, vec![
            // - 0: A -> a A_1
            // - 1: A -> e
            // - 2: A_1 -> b A_2
            // - 3: A_1 -> ε
            // - 4: A_2 -> c
            // - 5: A_2 -> d
            // - 6: A_2 -> ε
            (0, alt!(t 0, nt 1)),
            (0, alt!(t 4)),
            (1, alt!(t 1, nt 2)),
            (1, alt!(e)),
            (2, alt!(t 2)),
            (2, alt!(t 3)),
            (2, alt!(e)),
        ], vec![
            //     |  a   b   c   d   e   $
            // ----+-------------------------
            // A   |  0   .   .   .   1   p
            // A_1 |  .   2   .   .   .   3
            // A_2 |  .   .   4   5   .   6
              0,   7,   7,   7,   1,   8,
              7,   2,   7,   7,   7,   3,
              7,   7,   4,   5,   7,   6,
        ]),
        (T::PRS(43), 0, 0, vec![
            // BATCH -> GROUP ';' BATCH <L> | ε
            // GROUP -> '[' EXPR ']' | '(' EXPR ')'
            // EXPR -> FACTOR '*' FACTOR;
            // FACTOR -> id | int | '(' EXPR ')';
            //
            // first:                   follow:
            // -----------------------------------------
            // [ => [                   BATCH => $
            // ] => ]                   GROUP => $
            // ( => (                   EXPR => ] )
            // ) => )                   FACTOR => ] ) *
            // * => *
            // id => id
            // int => int
            // ; => ;
            // BATCH => [ ( ε
            // GROUP => [ (
            // EXPR => ( id int
            // FACTOR => ( id int
            // ε => ε
            //
            (0, alt!(nt 1, t 7, nt 0)),   // - 0: BATCH -> GROUP ; BATCH
            (0, alt!(e)),                 // - 1: BATCH -> ε
            (1, alt!(t 0, nt 2, t 1)),    // - 2: GROUP -> [ EXPR ]
            (1, alt!(t 2, nt 2, t 3)),    // - 3: GROUP -> ( EXPR )
            (2, alt!(nt 3, t 4, nt 3)),   // - 4: EXPR -> FACTOR * FACTOR
            (3, alt!(t 5)),               // - 5: FACTOR -> id
            (3, alt!(t 6)),               // - 6: FACTOR -> int
            (3, alt!(t 2, nt 2, t 3)),    // - 7: FACTOR -> ( EXPR )
        ], vec![
            //        |  [   ]   (   )   *  id  int  ;   $
            // -------+-------------------------------------
            // BATCH  |  0   .   0   .   .   .   .   .   1
            // GROUP  |  2   .   3   .   .   .   .   p   .
            // EXPR   |  .   p   4   p   .   4   4   .   .
            // FACTOR |  .   p   7   p   p   5   6   .   .
              0,   8,   0,   8,   8,   8,   8,   8,   1,
              2,   8,   3,   8,   8,   8,   8,   9,   8,
              8,   9,   4,   9,   8,   4,   4,   8,   8,
              8,   9,   7,   9,   9,   5,   6,   8,   8,
        ]),
        (T::PRS(51), 0, 0, vec![
            // E -> abs E | E ^ E | E ' | E * E | - E | E + E | F
            // F -> ( E ) | NUM | ID
            // - 0: E -> E_3 E_b
            // - 1: F -> ( E )
            // - 2: F -> NUM
            // - 3: F -> ID
            // - 4: E_b -> ^ E_3 E_b
            // - 5: E_b -> ' E_b
            // - 6: E_b -> * E_2 E_b
            // - 7: E_b -> + E_1 E_b
            // - 8: E_b -> ε
            // - 9: E_1 -> E_3 E_1b
            // - 10: E_1b -> ^ E_3 E_1b
            // - 11: E_1b -> ' E_1b
            // - 12: E_1b -> * E_2 E_1b
            // - 13: E_1b -> ε
            // - 14: E_2 -> E_3 E_2b
            // - 15: E_2b -> ^ E_3 E_2b
            // - 16: E_2b -> ' E_2b
            // - 17: E_2b -> ε
            // - 18: E_3 -> - E_1
            // - 19: E_3 -> abs E_3
            // - 20: E_3 -> F
            (0, alt!(nt 7, nt 2)),
            (1, alt!(t 5, nt 0, t 6)),
            (1, alt!(t 7)),
            (1, alt!(t 8)),
            (2, alt!(t 2, nt 7, nt 2)),
            (2, alt!(t 9, nt 2)),
            (2, alt!(t 3, nt 5, nt 2)),
            (2, alt!(t 4, nt 3, nt 2)),
            (2, alt!(e)),
            (3, alt!(nt 7, nt 4)),
            (4, alt!(t 2, nt 7, nt 4)),
            (4, alt!(t 9, nt 4)),
            (4, alt!(t 3, nt 5, nt 4)),
            (4, alt!(e)),
            (5, alt!(nt 7, nt 6)),
            (6, alt!(t 2, nt 7, nt 6)),
            (6, alt!(t 9, nt 6)),
            (6, alt!(e)),
            (7, alt!(t 1, nt 3)),
            (7, alt!(t 0, nt 7)),
            (7, alt!(nt 1)),
        ], vec![
            //      | abs  -   ^   *   +   (   )  NUM ID   '   $
            // -----+---------------------------------------------
            // E    |  0   0   .   .   .   0   p   0   0   .   p
            // F    |  .   .   p   p   p   1   p   2   3   p   p
            // E_b  |  .   .   4   6   7   .   8   .   .   5   8
            // E_1  |  9   9   p   p   p   9   p   9   9   p   p
            // E_1b |  .   .  10  12  13   .  13   .   .  11  13
            // E_2  | 14  14   p   p   p  14   p  14  14   p   p
            // E_2b |  .   .  15  17  17   .  17   .   .  16  17
            // E_3  | 19  18   p   p   p  20   p  20  20   p   p
              0,   0,  21,  21,  21,   0,  22,   0,   0,  21,  22,
             21,  21,  22,  22,  22,   1,  22,   2,   3,  22,  22,
             21,  21,   4,   6,   7,  21,   8,  21,  21,   5,   8,
              9,   9,  22,  22,  22,   9,  22,   9,   9,  22,  22,
             21,  21,  10,  12,  13,  21,  13,  21,  21,  11,  13,
             14,  14,  22,  22,  22,  14,  22,  14,  14,  22,  22,
             21,  21,  15,  17,  17,  21,  17,  21,  21,  16,  17,
             19,  18,  22,  22,  22,  20,  22,  20,  20,  22,  22,
        ]),
        (T::PRS(52), 0, 0, vec![
            // - 0: E -> E_4 E_1
            // - 1: F -> NUM
            // - 2: F -> ID
            // - 3: E_1 -> * E_4 E_1
            // - 4: E_1 -> ! E_1
            // - 5: E_1 -> ' E_1
            // - 6: E_1 -> + E_2 E_1
            // - 7: E_1 -> ε
            // - 8: E_2 -> E_4 E_3
            // - 9: E_3 -> <G> * E_4 E_3
            // - 10: E_3 -> <G> ! E_3
            // - 11: E_3 -> <G> ' E_3
            // - 12: E_3 -> ε
            // - 13: E_4 -> F
            (0, alt!(nt 5, nt 2)),
            (1, alt!(t 4)),
            (1, alt!(t 5)),
            (2, alt!(t 0, nt 5, nt 2)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(t 2, nt 2)),
            (2, alt!(t 3, nt 3, nt 2)),
            (2, alt!(e)),
            (3, alt!(nt 5, nt 4)),
            (4, alt!(t 0, nt 5, nt 4)),
            (4, alt!(t 1, nt 4)),
            (4, alt!(t 2, nt 4)),
            (4, alt!(e)),
            (5, alt!(nt 1)),
        ], vec![
            //     |  *   !   '   +  NUM ID   $
            // ----+-----------------------------
            // E   |  .   .   .   .   0   0   p
            // F   |  p   p   p   p   1   2   p
            // E_1 |  3   4   5   6   .   .   7
            // E_2 |  p   p   p   p   8   8   p
            // E_3 |  9  10  11  12   .   .  12
            // E_4 |  p   p   p   p  13  13   p
             14,  14,  14,  14,   0,   0,  15,
             15,  15,  15,  15,   1,   2,  15,
              3,   4,   5,   6,  14,  14,   7,
             15,  15,  15,  15,   8,   8,  15,
              9,  10,  11,  12,  14,  14,  12,
             15,  15,  15,  15,  13,  13,  15,
        ]),
        (T::PRS(53), 0, 0, vec![
            // E -> <R>E ^ E | <R>E * E | - E | E + E | F
            // F -> ID | NUM | ( E )
            // - 0: E -> E_3 E_b
            // - 1: F -> ID
            // - 2: F -> NUM
            // - 3: F -> ( E )
            // - 4: E_b -> ^ E_2 E_b     R-assoc (256)
            // - 5: E_b -> * E_1 E_b     R-assoc (256)
            // - 6: E_b -> + E_1 E_b
            // - 7: E_b -> ε
            // - 8: E_1 -> E_3 E_1b
            // - 9: E_1b -> ^ E_2 E_1b     R-assoc
            // - 10: E_1b -> * E_1 E_1b     R-assoc
            // - 11: E_1b -> ε
            // - 12: E_2 -> E_3 E_2b
            // - 13: E_2b -> ^ E_2 E_2b     R-assoc
            // - 14: E_2b -> ε
            // - 15: E_3 -> - E_1
            // - 16: E_3 -> F
            (0, alt!(nt 7, nt 2)),
            (1, alt!(t 4)),
            (1, alt!(t 5)),
            (1, alt!(t 6, nt 0, t 7)),
            (2, alt!(#R, t 0, nt 5, nt 2)),
            (2, alt!(#R, t 1, nt 3, nt 2)),
            (2, alt!(t 3, nt 3, nt 2)),
            (2, alt!(e)),
            (3, alt!(nt 7, nt 4)),
            (4, alt!(#R, t 0, nt 5, nt 4)),
            (4, alt!(#R, t 1, nt 3, nt 4)),
            (4, alt!(e)),
            (5, alt!(nt 7, nt 6)),
            (6, alt!(#R, t 0, nt 5, nt 6)),
            (6, alt!(e)),
            (7, alt!(t 2, nt 3)),
            (7, alt!(nt 1)),
        ], vec![
            //      |  ^   *   -   +  ID  NUM  (   )   $
            // -----+-------------------------------------
            // E    |  .   .   0   .   0   0   0   p   p
            // F    |  p   p   .   p   1   2   3   p   p
            // E_b  |  4   5   .   6   .   .   .   7   7
            // E_1  |  p   p   8   p   8   8   8   p   p
            // E_1b |  9  10   .  11   .   .   .  11  11
            // E_2  |  p   p  12   p  12  12  12   p   p
            // E_2b | 13  14   .  14   .   .   .  14  14
            // E_3  |  p   p  15   p  16  16  16   p   p
             17,  17,   0,  17,   0,   0,   0,  18,  18,
             18,  18,  17,  18,   1,   2,   3,  18,  18,
              4,   5,  17,   6,  17,  17,  17,   7,   7,
             18,  18,   8,  18,   8,   8,   8,  18,  18,
              9,  10,  17,  11,  17,  17,  17,  11,  11,
             18,  18,  12,  18,  12,  12,  12,  18,  18,
             13,  14,  17,  14,  17,  17,  17,  14,  14,
             18,  18,  15,  18,  16,  16,  16,  18,  18,
        ]),
        (T::PRS(54), 0, 0, vec![
            // E -> <R>E * E | E ! | E -- | <R>E + E | ID | NUM
            // - 0: E -> E_2 E_b
            // - 1: E_b -> * E_1 E_b     R-assoc
            // - 2: E_b -> ! E_b
            // - 3: E_b -> -- E_b
            // - 4: E_b -> + E E_b     R-assoc
            // - 5: E_b -> ε
            // - 6: E_1 -> E_2 E_1b
            // - 7: E_1b -> * E_1 E_1b     R-assoc
            // - 8: E_1b -> ε
            // - 9: E_2 -> ID
            // - 10: E_2 -> NUM
            (0, alt!(nt 4, nt 1)),
            (1, alt!(#R, t 1, nt 2, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(t 2, nt 1)),
            (1, alt!(#R, t 3, nt 0, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 4, nt 3)),
            (3, alt!(#R, t 1, nt 2, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 4)),
            (4, alt!(t 5)),
        ], vec![
            //      |  !   *  --   +  ID  NUM  $
            // -----+-----------------------------
            // E    |  p   p   p   p   0   0   p
            // E_b  |  2   1   3   4   .   .   5
            // E_1  |  p   p   p   p   6   6   p
            // E_1b |  8   7   8   8   .   .   8
            // E_2  |  p   p   p   p   9  10   p
             12,  12,  12,  12,   0,   0,  12,
              2,   1,   3,   4,  11,  11,   5,
             12,  12,  12,  12,   6,   6,  12,
              8,   7,   8,   8,  11,  11,   8,
             12,  12,  12,  12,   9,  10,  12,
        ]),
        (T::PRS(55), 0, 0, vec![
            // E -> E * E | E -- | ! E | E + E | ID | NUM
            // - 0: E -> E_2 E_b
            // - 1: E_b -> * E_2 E_b
            // - 2: E_b -> -- E_b
            // - 3: E_b -> + E_1 E_b
            // - 4: E_b -> ε
            // - 5: E_1 -> E_2 E_1b
            // - 6: E_1b -> * E_2 E_1b
            // - 7: E_1b -> -- E_1b
            // - 8: E_1b -> ε
            // - 9: E_2 -> ! E_1
            // - 10: E_2 -> ID
            // - 11: E_2 -> NUM
            (0, alt!(nt 4, nt 1)),
            (1, alt!(t 1, nt 4, nt 1)),
            (1, alt!(t 2, nt 1)),
            (1, alt!(t 3, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 4, nt 3)),
            (3, alt!(t 1, nt 4, nt 3)),
            (3, alt!(t 2, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 0, nt 2)),
            (4, alt!(t 4)),
            (4, alt!(t 5)),
        ], vec![
            //      |  !   *  --   +  ID  NUM  $
            // -----+-----------------------------
            // E    |  0   .   .   .   0   0   p
            // E_b  |  .   1   2   3   .   .   4
            // E_1  |  5   p   p   p   5   5   p
            // E_1b |  .   6   7   8   .   .   8
            // E_2  |  9   p   p   p  10  11   p
              0,  12,  12,  12,   0,   0,  13,
             12,   1,   2,   3,  12,  12,   4,
              5,  13,  13,  13,   5,   5,  13,
             12,   6,   7,   8,  12,  12,   8,
              9,  13,  13,  13,  10,  11,  13,
        ]),
        (T::PRS(56), 0, 0, vec![
            // E -> E * E | ! E | E -- | E + E | ID | NUM
            // - 0: E -> E_3 E_b
            // - 1: E_b -> * E_3 E_b
            // - 2: E_b -> -- E_b
            // - 3: E_b -> + E_1 E_b
            // - 4: E_b -> ε
            // - 5: E_1 -> E_3 E_1b
            // - 6: E_1b -> * E_3 E_1b
            // - 7: E_1b -> -- E_1b
            // - 8: E_1b -> ε
            // - 9: E_2 -> E_3 E_2b
            // - 10: E_2b -> * E_3 E_2b
            // - 11: E_2b -> ε
            // - 12: E_3 -> ! E_2
            // - 13: E_3 -> ID
            // - 14: E_3 -> NUM
            (0, alt!(nt 6, nt 1)),
            (1, alt!(t 1, nt 6, nt 1)),
            (1, alt!(t 2, nt 1)),
            (1, alt!(t 3, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 6, nt 3)),
            (3, alt!(t 1, nt 6, nt 3)),
            (3, alt!(t 2, nt 3)),
            (3, alt!(e)),
            (4, alt!(nt 6, nt 5)),
            (5, alt!(t 1, nt 6, nt 5)),
            (5, alt!(e)),
            (6, alt!(t 0, nt 4)),
            (6, alt!(t 4)),
            (6, alt!(t 5)),
        ], vec![
            //      |  !   *  --   +  ID  NUM  $
            // -----+-----------------------------
            // E    |  0   .   .   .   0   0   p
            // E_b  |  .   1   2   3   .   .   4
            // E_1  |  5   p   p   p   5   5   p
            // E_1b |  .   6   7   8   .   .   8
            // E_2  |  9   p   p   p   9   9   p
            // E_2b |  .  10  11  11   .   .  11
            // E_3  | 12   p   p   p  13  14   p
              0,  15,  15,  15,   0,   0,  16,
             15,   1,   2,   3,  15,  15,   4,
              5,  16,  16,  16,   5,   5,  16,
             15,   6,   7,   8,  15,  15,   8,
              9,  16,  16,  16,   9,   9,  16,
             15,  10,  11,  11,  15,  15,  11,
             12,  16,  16,  16,  13,  14,  16,
        ]),
        (T::PRS(57), 0, 0, vec![
            // E -> E ^ E | E * E | E + E | ID | NUM
            // - 0: E -> E_3 E_b
            // - 1: E_b -> ^ E_3 E_b
            // - 2: E_b -> * E_2 E_b
            // - 3: E_b -> + E_1 E_b
            // - 4: E_b -> ε
            // - 5: E_1 -> E_3 E_1b
            // - 6: E_1b -> ^ E_3 E_1b
            // - 7: E_1b -> * E_2 E_1b
            // - 8: E_1b -> ε
            // - 9: E_2 -> E_3 E_2b
            // - 10: E_2b -> ^ E_3 E_2b
            // - 11: E_2b -> ε
            // - 12: E_3 -> ID
            // - 13: E_3 -> NUM
            (0, alt!(nt 6, nt 1)),
            (1, alt!(t 0, nt 6, nt 1)),
            (1, alt!(t 1, nt 4, nt 1)),
            (1, alt!(t 2, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 6, nt 3)),
            (3, alt!(t 0, nt 6, nt 3)),
            (3, alt!(t 1, nt 4, nt 3)),
            (3, alt!(e)),
            (4, alt!(nt 6, nt 5)),
            (5, alt!(t 0, nt 6, nt 5)),
            (5, alt!(e)),
            (6, alt!(t 3)),
            (6, alt!(t 4)),
        ], vec![
            //      |  ^   *   +  ID  NUM  $
            // -----+-------------------------
            // E    |  .   .   .   0   0   p
            // E_b  |  1   2   3   .   .   4
            // E_1  |  p   p   p   5   5   p
            // E_1b |  6   7   8   .   .   8
            // E_2  |  p   p   p   9   9   p
            // E_2b | 10  11  11   .   .  11
            // E_3  |  p   p   p  12  13   p
             14,  14,  14,   0,   0,  15,
              1,   2,   3,  14,  14,   4,
             15,  15,  15,   5,   5,  15,
              6,   7,   8,  14,  14,   8,
             15,  15,  15,   9,   9,  15,
             10,  11,  11,  14,  14,  11,
             15,  15,  15,  12,  13,  15,
        ]),
        (T::PRS(66), 0, 0, vec![
            // E -> E . * E | E -- | E . + E | ! E | ID
            // - 0: E -> E_4 E_1
            // - 1: E_1 -> <G> -- E_1
            // - 2: E_1 -> <G> . E_5
            // - 3: E_1 -> ε
            // - 4: E_2 -> E_4 E_3
            // - 5: E_3 -> <G> . * E_4 E_3
            // - 6: E_3 -> <G> -- E_3
            // - 7: E_3 -> ε
            // - 8: E_4 -> ! E
            // - 9: E_4 -> ID
            // - 10: E_5 -> <G> * E_4 E_1
            // - 11: E_5 -> <G> + E_2 E_1
            (0, alt!(nt 4, nt 1)),
            (1, alt!(t 1, nt 1)),
            (1, alt!(t 4, nt 5)),
            (1, alt!(e)),
            (2, alt!(nt 4, nt 3)),
            (3, alt!(t 4, t 0, nt 4, nt 3)),
            (3, alt!(t 1, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 3, nt 0)),
            (4, alt!(t 5)),
            (5, alt!(t 0, nt 4, nt 1)),
            (5, alt!(t 2, nt 2, nt 1)),
        ], vec![
            //     |  *  --   +   !   .  ID   $
            // ----+-----------------------------
            // E   |  .   p   .   0   p   0   p
            // E_1 |  .   1   .   .   2   .   3
            // E_2 |  .   p   .   4   p   4   p
            // E_3 |  .   6   .   .   5   .   7
            // E_4 |  .   p   .   8   p   9   p
            // E_5 | 10   p  11   .   p   .   p
             12,  13,  12,   0,  13,   0,  13,
             12,   1,  12,  12,   2,  12,   3,
             12,  13,  12,   4,  13,   4,  13,
             12,   6,  12,  12,   5,  12,   7,
             12,  13,  12,   8,  13,   9,  13,
             10,  13,  11,  12,  13,  12,  13,
        ]),
        (T::PRS(58), 0, 0, vec![
            // E -> E + | - E | 0
            // - 0: E -> - E
            // - 1: E -> 0 E_1
            // - 2: E_1 -> + E_1
            // - 3: E_1 -> ε
            (0, alt!(t 1, nt 0)),
            (0, alt!(t 2, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  +   -   0   $
            // ----+-----------------
            // E   |  .   0   1   p
            // E_1 |  2   .   .   3
              4,   0,   1,   5,
              2,   4,   4,   3,
        ]),
        (T::PRS(61), 0, 0, vec![
            // E -> E + | - E | 0 | 1
            // - 0: E -> - E
            // - 1: E -> 0 E_1
            // - 2: E -> 1 E_1
            // - 3: E_1 -> + E_1
            // - 4: E_1 -> ε
            (0, alt!(t 1, nt 0)),
            (0, alt!(t 2, nt 1)),
            (0, alt!(t 3, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  +   -   0   1   $
            // ----+---------------------
            // E   |  .   0   1   2   p
            // E_1 |  3   .   .   .   4
              5,   0,   1,   2,   6,
              3,   5,   5,   5,   4,
        ]),
        (T::PRS(70), 0, 0, vec![
            // E -> - E | E + | 0
            // - 0: E -> E_1 E_b
            // - 1: E_b -> + E_b
            // - 2: E_b -> ε
            // - 3: E_1 -> - E_1
            // - 4: E_1 -> 0
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(t 2)),
        ], vec![
            //     |  +   -   0   $
            // ----+-----------------
            // E   |  .   0   0   p
            // E_b |  1   .   .   2
            // E_1 |  p   3   4   p
              5,   0,   0,   6,
              1,   5,   5,   2,
              6,   3,   4,   6,
        ]),
        (T::PRS(59), 0, 0, vec![
            // E -> E + E | - E | 0
            // - 0: E -> E_1 E_b
            // - 1: E_b -> + E_1 E_b
            // - 2: E_b -> ε
            // - 3: E_1 -> - E
            // - 4: E_1 -> 0
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 0)),
            (2, alt!(t 2)),
        ], vec![
            //     |  +   -   0   $
            // ----+-----------------
            // E   |  p   0   0   p
            // E_b |  1   .   .   2
            // E_1 |  p   3   4   p
              6,   0,   0,   6,
              1,   5,   5,   2,
              6,   3,   4,   6,
        ]),
        (T::PRS(64), 0, 0, vec![
            // E -> - E | E + E | 0
            // - 0: E -> E_1 E_b
            // - 1: E_b -> + E_1 E_b
            // - 2: E_b -> ε
            // - 3: E_1 -> - E_1
            // - 4: E_1 -> 0
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(t 2)),
        ], vec![
            //     |  +   -   0   $
            // ----+-----------------
            // E   |  .   0   0   p
            // E_b |  1   .   .   2
            // E_1 |  p   3   4   p
              5,   0,   0,   6,
              1,   5,   5,   2,
              6,   3,   4,   6,
        ]),
        (T::PRS(63), 0, 0, vec![
            // E -> <R>E ^ E | E * E | - E | E + E | ID
            // - 0: E -> E_1b E3
            // - 1: E3 -> ^ E_b E3     R-assoc (256)
            // - 2: E3 -> * E_b E3
            // - 3: E3 -> + E5 E3
            // - 4: E3 -> ε
            // - 5: E5 -> E_1b E6
            // - 6: E6 -> ^ E_b E6     R-assoc
            // - 7: E6 -> * E_b E6
            // - 8: E6 -> ε
            // - 9: E_b -> E_1b E_1
            // - 10: E_1 -> ^ E_b E_1     R-assoc
            // - 11: E_1 -> ε
            // - 12: E_1b -> - E5
            // - 13: E_1b -> ID
            (0, alt!(nt 6, nt 1)),
            (1, alt!(#R, t 0, nt 4, nt 1)),
            (1, alt!(t 1, nt 4, nt 1)),
            (1, alt!(t 3, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 6, nt 3)),
            (3, alt!(#R, t 0, nt 4, nt 3)),
            (3, alt!(t 1, nt 4, nt 3)),
            (3, alt!(e)),
            (4, alt!(nt 6, nt 5)),
            (5, alt!(#R, t 0, nt 4, nt 5)),
            (5, alt!(e)),
            (6, alt!(t 2, nt 2)),
            (6, alt!(t 4)),
        ], vec![
            //      |  ^   *   -   +  ID   $
            // -----+-------------------------
            // E    |  .   .   0   .   0   p
            // E3   |  1   2   .   3   .   4
            // E5   |  p   p   5   p   5   p
            // E6   |  6   7   .   8   .   8
            // E_b  |  p   p   9   p   9   p
            // E_1  | 10  11   .  11   .  11
            // E_1b |  p   p  12   p  13   p
             14,  14,   0,  14,   0,  15,
              1,   2,  14,   3,  14,   4,
             15,  15,   5,  15,   5,  15,
              6,   7,  14,   8,  14,   8,
             15,  15,   9,  15,   9,  15,
             10,  11,  14,  11,  14,  11,
             15,  15,  12,  15,  13,  15,
        ]),
        (T::PRS(65), 0, 0, vec![
            // E -> E ! | E * E | E + | - E | ID
            // - 0: E -> E_2 E_b
            // - 1: E_b -> ! E_b
            // - 2: E_b -> * E_1 E_b
            // - 3: E_b -> + E_b
            // - 4: E_b -> ε
            // - 5: E_1 -> E_2 E_1b
            // - 6: E_1b -> ! E_1b
            // - 7: E_1b -> ε
            // - 8: E_2 -> - E
            // - 9: E_2 -> ID
            (0, alt!(nt 4, nt 1)),
            (1, alt!(t 1, nt 1)),
            (1, alt!(t 0, nt 2, nt 1)),
            (1, alt!(t 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 4, nt 3)),
            (3, alt!(t 1, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 3, nt 0)),
            (4, alt!(t 4)),
        ], vec![
            //      |  *   !   +   -  ID   $
            // -----+-------------------------
            // E    |  p   p   p   0   0   p
            // E_b  |  2   1   3   .   .   4
            // E_1  |  p   p   p   5   5   p
            // E_1b |  7   6   7   .   .   7
            // E_2  |  p   p   p   8   9   p
             11,  11,  11,   0,   0,  11,
              2,   1,   3,  10,  10,   4,
             11,  11,  11,   5,   5,  11,
              7,   6,   7,  10,  10,   7,
             11,  11,  11,   8,   9,  11,
        ]),

        (T::PRS(100), 0, 0, vec![
            // - 0: A -> c A_1
            // - 1: A_1 -> a A b A_1
            // - 2: A_1 -> ε
            (0, alt!(t 2, nt 1)),
            (1, alt!(t 0, nt 0, t 1, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  a   b   c   $
            // ----+-----------------
            // A   |  .   p   0   p
            // A_1 |  1   2   .   2
              3,   4,   0,   4,
              1,   2,   3,   2,
        ]),
        (T::PRS(101), 0, 0, vec![
            // - 0: A -> a A A
            // - 1: A -> b
            (0, alt!(t 0, nt 0, nt 0)),
            (0, alt!(t 1)),
        ], vec![
            //   |  a   b   $
            // --+-------------
            // A |  0   1   p
              0,   1,   3,
        ]),
        (T::PRS(102), 0, 0, vec![
            // - 0: A -> A_2 A_1
            // - 1: A_1 -> a A b A_2 A_1
            // - 2: A_1 -> ε
            // - 3: A_2 -> c
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 0, t 1, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 2)),
        ], vec![
            //     |  a   b   c   $
            // ----+-----------------
            // A   |  .   p   0   p
            // A_1 |  1   2   .   2
            // A_2 |  p   p   3   p
              4,   5,   0,   5,
              1,   2,   4,   2,
              5,   5,   3,   5,
        ]),
        (T::PRS(103), 0, 0, vec![
            // - 0: A -> a B c
            // - 1: A -> d
            // - 2: B -> A b A B
            // - 3: B -> ε
            (0, alt!(t 0, nt 1, t 2)),
            (0, alt!(t 3)),
            (1, alt!(nt 0, t 1, nt 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //   |  a   b   c   d   $
            // --+---------------------
            // A |  0   p   p   1   p
            // B |  2   .   3   2   .
              0,   5,   5,   1,   5,
              2,   4,   3,   2,   4,
        ]),
        (T::PRS(104), 0, 2, vec![
            // - 0: A -> B c
            // - 1: A -> a
            // - 2: B -> A b A B
            // - 3: B -> ε
            (0, alt!(nt 1, t 2)),
            (0, alt!(t 0)),
            (1, alt!(nt 0, t 1, nt 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //   |  a   b   c   $
            // --+-----------------
            // A |  1   p   0   p
            // B |  2   .   3   .
              1,   5,   0,   5,
              2,   4,   3,   4,
            // calc_table: ambiguity for NT 'A', T 'a': <B c> or <a> => <a> has been chosen
            // calc_table: ambiguity for NT 'B', T 'c': <A b A B> or <ε> => <ε> has been chosen
        ]),
        (T::PRS(105), 0, 2, vec![
            // - 0: A -> a B
            // - 1: A -> c
            // - 2: B -> A b A B
            // - 3: B -> ε
            (0, alt!(t 0, nt 1)),
            (0, alt!(t 2)),
            (1, alt!(nt 0, t 1, nt 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //   |  a   b   c   $
            // --+-----------------
            // A |  0   p   1   p
            // B |  2   3   2   3
              0,   5,   1,   5,
              2,   3,   2,   3,
            // calc_table: ambiguity for NT 'B', T 'a': <A b A B> or <ε> => <A b A B> has been chosen
            // calc_table: ambiguity for NT 'B', T 'c': <A b A B> or <ε> => <A b A B> has been chosen
        ]),
        (T::PRS(106), 0, 4, vec![
            // - 0: A -> B
            // - 1: A -> a
            // - 2: B -> A b A B
            // - 3: B -> ε
            (0, alt!(nt 1)),
            (0, alt!(t 0)),
            (1, alt!(nt 0, t 1, nt 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //   | a  b  $
            // --+----------
            // A | 1  0  0
            // B | 2  2  3
              1,   0,   0,
              2,   2,   3,
            // calc_table: ambiguity for NT 'A', T 'a': <B> or <B> or <a> => <a> has been chosen
            // calc_table: ambiguity for NT 'A', T 'b': <B> or <B> => <B> has been chosen
            // calc_table: ambiguity for NT 'B', T 'a': <A b A B> or <ε> => <A b A B> has been chosen
            // calc_table: ambiguity for NT 'B', T 'b': <A b A B> or <ε> => <A b A B> has been chosen
        ]),
        (T::RTS(56), 0, 1, vec![
            // A -> (a b)* a
            // - 0: A -> A_1 a
            // - 1: A_1 -> a b A_1
            // - 2: A_1 -> ε
            (0, alt!(nt 1, t 0)),
            (1, alt!(t 0, t 1, nt 1)),
            (1, alt!(e)),

        ], vec![
            //     |  a   b   $
            // ----+-------------
            // A   |  0   .   p
            // A_1 |  1   .   .
              0,   3,   4,
              1,   3,   3,
            // calc_table: ambiguity for NT 'A_1', T 'a': <a b A_1> or <ε> => <a b A_1> has been chosen
        ]),
        (T::RTS(57), 0, 0, vec![
            // - 0: A -> a A_1
            // - 1: A_1 -> b a A_1
            // - 2: A_1 -> ε
            (0, alt!(t 0, nt 1)),
            (1, alt!(t 1, t 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  a   b   $
            // ----+-------------
            // A   |  0   .   p
            // A_1 |  .   1   2
              0,   3,   4,
              3,   1,   2,
        ]),
        // (T::RTS(57), 0, 0, vec![
        //     // - 0: A -> a A_1
        //     // - 1: A_1 -> b a A_1
        //     // - 2: A_1 -> ε
        //     (0, alt!(t 0, nt 1)),
        //     (1, alt!(t 1, t 0, nt 1)),
        //     (1, alt!(e)),
        // ], vec![
        //     //     |  a   b   $
        //     // ----+-------------
        //     // A   |  0   .   p
        //     // A_1 |  .   1   2
        //       0,   3,   4,
        //       3,   1,   2,
        // ]),
        // (T::RTS(57), 0, 0, vec![
        // ], vec![
        // ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (rule_id, start, expected_warnings, expected_alts, expected_table)) in tests.into_iter().enumerate() {
        let mut ll1 = rule_id.build_prs(test_id, start, false);
        if VERBOSE {
            println!("{:=<80}\ntest {test_id} with {rule_id:?}/{start}:", "");
            ll1.print_rules(false, false);
        }
        let first = ll1.calc_first();
        let follow = ll1.calc_follow(&first);
        if VERBOSE {
            map_and_print_first(&first, ll1.get_symbol_table());
            map_and_print_follow(&follow, ll1.get_symbol_table());
        }
        let parsing_table = ll1.calc_table(&first, &follow, true);
        let LLParsingTable { num_nt, num_t, alts, table, .. } = &parsing_table;
        assert_eq!(num_nt * num_t, table.len(), "incorrect table size in test {test_id}/{rule_id:?}/{start}");
        if VERBOSE {
            println!("num_nt = {num_nt}, num_t = {num_t}");
            ll1.print_rules(false, false);
            print_alts(&alts, ll1.get_symbol_table());
            println!("{}",
                     alts.iter().enumerate().map(|(_id, (v, f))| {
                         let flags = if f.get_flags() != 0 {
                             let mut vf = ruleflag::alt_info_to_string(f.get_flags());
                             format!("#{}, ", if vf.len() == 1 { vf.pop().unwrap() } else { f.get_flags().to_string() })
                         } else {
                             String::new()
                         };
                         format!("            ({v}, alt!({flags}{})),", f.iter().map(|s| s.to_macro_item()).join(", "))
                     }
            ).join("\n"));
            println!("table:");
            parsing_table.print(ll1.get_symbol_table(), 12);
            for i in 0..*num_nt {
                println!("            {},", (0..*num_t).map(|j| format!("{:3}", table[i * num_t + j])).join(", "));
            }
            if table.len() < 30 {
                println!("vec![{}]", table.iter().map(|x| x.to_string()).join(", "));
            }
            println!("flags:");
            for v in 0..ll1.num_nt as VarId {
                let parent = ll1.get_parent(v).map(|p| ll1.get_symbol_table().unwrap().get_nt_name(p));
                println!("- {}: {}{}",
                         ll1.get_symbol_table().unwrap().get_nt_name(v),
                         ruleflag::to_string(ll1.get_flags(v)).join(" "),
                         if let Some(p) = parent { format!(", parent: {p}") } else { String::new() } );
            }
            ll1.print_logs();
        }
        assert_eq!(*alts, expected_alts, "test {test_id}/{rule_id:?}/{start} failed");
        assert_eq!(*table, expected_table, "test {test_id}/{rule_id:?}/{start} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id}/{rule_id:?}/{start} failed on # errors");
        assert_eq!(ll1.log.num_warnings(), expected_warnings, "test {test_id}/{rule_id:?}/{start} failed, warnings: {}", ll1.log.get_warnings().join("\n"));
   }
}

#[test]
fn build_prs_error() {
    let rts = build_rts(101);
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

// ---------------------------------------------------------------------------------------------
// Tests the chain from RTS to LL1 grammar

fn build_ll1_from_rts(id: u32) -> ProdRuleSet<LL1> {
    let mut rts = RuleTreeSet::new();
    let mut symbol_table = SymbolTable::new();
    let start = Some(0);
    let mut tree = vec![GrTree::new()];
    match id {
        100 => {
            // A -> a b | c
            let or = tree[0].add_root(gnode!(|));
            let cc = tree[0].add(Some(or), gnode!(&));
            tree[0].add_iter(Some(cc), [gnode!(t 0), gnode!(t 1)]);
            tree[0].add(Some(or), gnode!(t 2));
        }
        102 => {
            // A -> a (b B | c)+
            // B -> a
            tree.push(GrTree::new());
            let cc = tree[0].add_root(gnode!(&));
            tree[0].add(Some(cc), gnode!(t 0));
            let p = tree[0].add(Some(cc), gnode!(+));
            let or = tree[0].add(Some(p), gnode!(|));
            tree[0].addc_iter(Some(or), gnode!(&), [gnode!(t 1), gnode!(nt 1)]);
            tree[0].add(Some(or), gnode!(t 2));
            tree[1].add_root(gnode!(t 0));
        }
        _ => {}
    }
    let num_nt = tree.len();
    for (i, t) in tree.into_iter().index() {
        rts.set_tree(i, t);
    }
    let num_t = rts.get_terminals().iter().max().map(|n| *n + 1).unwrap_or(0);
    if symbol_table.get_num_t() == 0 {
        symbol_table.extend_terminals((0..num_t).map(|i| (format!("{}", char::from(i as u8 + 97)), None)));
    }
    if symbol_table.get_num_nt() == 0 {
        assert!(num_nt <= 26);
        symbol_table.extend_nonterminals((0..num_nt as u8).map(|i| format!("{}", char::from(i + 65))));
    }
    rts.set_symbol_table(symbol_table);
    if let Some(start) = start {
        rts.set_start(start);
    }

    let rules = ProdRuleSet::<General>::build_from(rts);
    ProdRuleSet::<LL1>::build_from(rules)
}

#[test]
fn rts_prs() {
    let tests = vec![
        (100, 0, vec![
            // - 0: A -> a b
            // - 1: A -> c
            (0, alt!(t 0, t 1)),
            (0, alt!(t 2)),
        ]),
        (102, 0, vec![
            // A -> A a A b A | c
            //
            // - 0: A -> a A_1
            // - 1: B -> a
            // - 2: A_1 -> b B A_2
            // - 3: A_1 -> c A_3
            // - 4: A_2 -> A_1
            // - 5: A_2 -> ε
            // - 6: A_3 -> A_1
            // - 7: A_3 -> ε
            (0, alt!(t 0, nt 2)),
            (1, alt!(t 0)),
            (2, alt!(t 1, nt 1, nt 3)),
            (2, alt!(t 2, nt 4)),
            (3, alt!(nt 2)),
            (3, alt!(e)),
            (4, alt!(nt 2)),
            (4, alt!(e)),
        ])
    ];
    const VERBOSE: bool = false;
    for (ll_id, start, expected_alts) in tests {
        let mut ll1 = build_ll1_from_rts(ll_id);
        ll1.set_start(start);
        let first = ll1.calc_first();
        let follow = ll1.calc_follow(&first);
        let parsing_table = ll1.calc_table(&first, &follow, false);
        let LLParsingTable { alts, .. } = &parsing_table;
        if VERBOSE {
            print_alts(&alts, ll1.get_symbol_table());
            println!("{}",
                     alts.iter().enumerate().map(|(_id, (v, f))|
                         format!("            ({v}, alt!({})),", f.iter().map(|s| s.to_macro_item()).join(", "))
                     ).join("\n"));
        }
        assert_eq!(*alts, expected_alts, "test {ll_id}/{start} failed");
    }
}

// ---------------------------------------------------------------------------------------------
// Pre-parser

#[test]
fn rts_prs_flags() {
    let tests: Vec<(T, VarId, BTreeMap<VarId, u32>, BTreeMap<usize, u32>, BTreeMap<VarId, VarId>, BTreeMap<VarId, NTConversion>)> = vec![
        (T::RTS(9), 0,btreemap![0 => 6144, 1 => 4129, 2 => 64],     // NT flags
         btreemap![],                                               // alternative flags
         btreemap![1 => 0, 2 => 1],                                 // parents
         btreemap![]),
        (T::RTS(11), 0, btreemap![0 => 2048, 1 => 1],
         btreemap![],
         btreemap![1 => 0],
         btreemap![]),
        (T::RTS(12), 0, btreemap![0 => 2048, 1 => 1],
         btreemap![],
         btreemap![1 => 0],
         btreemap![]),
        (T::RTS(15), 0, btreemap![0 => 1536, 1 => 4, 2 => 512, 3 => 4],
         btreemap![2 => 256, 6 => 8192, 7 => 8448],
         btreemap![1 => 0, 2 => 0, 3 => 2, 4 => 0],
         btreemap![]),
        (T::RTS(16), 0, btreemap![0 => 6656, 1 => 4129, 2 => 4, 3 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 0, 3 => 1],
         btreemap![]),
        (T::RTS(17), 0, btreemap![0 => 6144, 1 => 4129, 2 => 6177, 3 => 64, 4 => 64],
         btreemap![],
         btreemap![1 => 2, 2 => 0, 3 => 1, 4 => 2],
         btreemap![]),
        (T::RTS(18), 0, btreemap![], btreemap![], btreemap![],
         btreemap![]),
        // 0: A (b <L=B>)* c | d       0: A -> d A_1
        // 1: (B)                  ->  1: AIter1 -> b AIter1 | ε
        // 2:                          2: A_1 -> AIter1 c A_1 | ε
        // - NT flags:
        //   - A: parent_left_rec | parent_+_or_* (2560)
        //   - AIter1: child_+_or_* | L-form (129)
        //   - A_1: child_left_rec (4)
        // - parents:
        //   - AIter1 -> A
        //   - A_1 -> A
        (T::RTS(19), 0, btreemap![0 => 2560, 1 => 129, 2 => 4],
         btreemap![],
         btreemap![1 => 0, 2 => 0],
         btreemap![]),
        (T::RTS(20), 0, btreemap![0 => 2560, 1 => 1, 2 => 4],
         btreemap![],
         btreemap![1 => 0, 2 => 0],
         btreemap![]),
        (T::RTS(29), 0, btreemap![0 => 2048, 2 => 1, 3 => 2049],
         btreemap![],
         btreemap![2 => 3, 3 => 0],
         btreemap![]),
        (T::RTS(35), 0, btreemap![0 => 2],
         btreemap![],
         btreemap![],
         btreemap![]),
        (T::RTS(36), 0, btreemap![0 => 130],
         btreemap![],
         btreemap![],
         btreemap![]),
        // 0: A -> a (b <L=AIter1>)* C      0: A -> a AIter1 C
        // 1: (AIter1)                  ->  1: C -> c
        // 2: C -> c                        2: AIter1 -> b AIter1 | ε
        // - NT flags:
        //   - A: parent_+_or_* (2048)
        //   - AIter1: child_+_or_* | L-form (129)
        // - parents:
        //   - AIter1 -> A
        (T::RTS(37), 0, btreemap![0 => 2048, 1 => 129],
         btreemap![],
         btreemap![1 => 0],
         btreemap![]),
        // - 0: A -> a AIter1 d
        // - 1: AIter2 -> b AIter2
        // - 2: AIter2 -> ε
        // - 3: AIter1 -> AIter2 c AIter1
        // - 4: AIter1 -> ε
        // - NT flags:
        //   - A: parent_+_or_* (2048)
        //   - AIter2: child_+_or_* | L-form (129)
        //   - AIter1: child_+_or_* | L-form | parent_+_or_* (2177)
        // - parents:
        //   - AIter2 -> AIter1
        //   - AIter1 -> A
        (T::RTS(39), 0, btreemap![0 => 2048, 1 => 129, 2 => 2177],
         btreemap![],
         btreemap![1 => 2, 2 => 0],
         btreemap![]),
        (T::PRS(0), 0, btreemap![0 => 544, 1 => 4, 2 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 0],
         btreemap![1 => Removed]),
        (T::PRS(1), 0, btreemap![0 => 32, 1 => 96, 2 => 64, 3 => 64, 4 => 64],
         btreemap![9 => 256],
         btreemap![1 => 0, 2 => 0, 3 => 1, 4 => 1],
         btreemap![]),
        (T::PRS(25), 0, btreemap![0 => 512, 1 => 36, 2 => 96, 3 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 1, 3 => 2],
         btreemap![]),
        (T::PRS(28), 0, btreemap![0 => 32, 1 => 96, 2 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 1],
         btreemap![]),
        (T::PRS(31), 0, btreemap![0 => 512, 2 => 4],
         btreemap![],
         btreemap![2 => 0],
         btreemap![]),
        (T::PRS(32), 0, btreemap![0 => 512, 2 => 36, 3 => 64],
         btreemap![],
         btreemap![2 => 0, 3 => 2],
         btreemap![]),
        (T::PRS(33), 0, btreemap![0 => 544, 1 => 4, 2 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 0],
         btreemap![]),
        (T::PRS(40), 0, btreemap![0 => 2],
         btreemap![],
         btreemap![],
         btreemap![]),
        (T::PRS(41), 0, btreemap![0 => 130],
         btreemap![],
         btreemap![],
         btreemap![]),
        (T::PRS(42), 0, btreemap![0 => 130],
         btreemap![],
         btreemap![],
         btreemap![]),
        (T::PRS(44), 0, btreemap![0 => 34, 2 => 64],
         btreemap![],
         btreemap![2 => 0],
         btreemap![]),
        /*
        (T::PRS(), 0, btreemap![], btreemap![], btreemap![], btreemap![]),
        */
    ];
    const VERBOSE: bool = false;
    const VERBOSE_DETAILS: bool = false;
    for (test_id, (rule_id, start_nt, expected_flags, expected_fflags, expected_parent, expected_nt_conversion)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {rule_id:?}, start {start_nt}:", ""); }
        let mut ll1 = rule_id.build_prs(test_id, start_nt, true);
        if VERBOSE && VERBOSE_DETAILS {
            print!("Before table creation:\n- ");
            ll1.print_prs_summary();
        }
        let _parsing_table = ll1.make_parsing_table(false);
        if VERBOSE && ll1.log.num_warnings() + ll1.log.num_notes() > 0 {
            ll1.print_logs();
        }
        let result_flags = ll1.flags.iter().index().filter_map(|(v, &f)| if f != 0 { Some((v, f)) } else { None }).collect::<BTreeMap<_, _>>();
        let result_fflags = ll1.prules.iter().flat_map(|p| p.iter().map(|f| f.get_flags())).enumerate().filter_map(|(i, f)| if f != 0 { Some((i, f)) } else { None }).collect::<BTreeMap<_, _>>();
        let result_parent = ll1.parent.iter().index().filter_map(|(v, &par)| if let Some(p) = par { Some((v, p)) } else { None }).collect::<BTreeMap<_, _>>();
        let result_nt_conversion = ll1.nt_conversion.iter().map(|(v1, v2)| (*v1, *v2)).collect::<BTreeMap<_, _>>();
        if VERBOSE {
            print!("- ");
            ll1.print_prs_summary();
            println!("=>");
            println!("        (T::{rule_id:?}, {start_nt}, btreemap![{}],", result_flags.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
            println!("         btreemap![{}],", result_fflags.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
            println!("         btreemap![{}],", result_parent.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
            println!("         btreemap![{}]),", result_nt_conversion.iter().map(|(v1, v2)| format!("{v1} => {v2:?}")).join(", "));
        }
        assert_eq!(result_flags, expected_flags, "test {test_id}/{rule_id:?}/{start_nt} failed");
        assert_eq!(result_fflags, expected_fflags, "test {test_id}/{rule_id:?}/{start_nt} failed");
        assert_eq!(result_parent, expected_parent, "test {test_id}/{rule_id:?}/{start_nt} failed");
        assert_eq!(result_nt_conversion, expected_nt_conversion, "test {test_id}/{rule_id:?}/{start_nt} failed");
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub(crate) enum T { RTS(u32), PRS(u32) }

impl T {
    /// Build a PRS from RTS or PRS rules, does not verify if there are errors in the log
    pub(crate) fn try_build_prs(&self, start_nt: VarId, is_t_data: bool) -> ProdRuleSet<LL1> {
        const VERBOSE: bool = false;
        let mut ll1 = match self {
            T::RTS(id) => {
                let mut rts = rts::build_rts(*id);
                if rts.get_symbol_table().is_none() {
                    let num_nt = rts.trees.len();
                    let num_t = rts.get_terminals().iter().map(|token| *token as usize).max().unwrap_or(0) + 1;
                    let mut symbol_table = SymbolTable::new();
                    complete_symbol_table(&mut symbol_table, num_t, num_nt, is_t_data);
                    rts.set_symbol_table(symbol_table);
                }
                let rules = ProdRuleSet::build_from(rts);
                if VERBOSE {
                    print!("General rules\n- ");
                    rules.print_prs_summary();
                }
                ProdRuleSet::<LL1>::build_from(rules)
            }
            T::PRS(id) => {
                let general = build_prs(*id, is_t_data);
                if VERBOSE {
                    print!("General rules\n- ");
                    general.print_prs_summary();
                }
                ProdRuleSet::<LL1>::build_from(general)
            }
        };
        ll1.set_start(start_nt);
        ll1
    }

    /// Build a PRS from RTS or PRS rules and verifies there are no errors in the log
    pub(crate) fn build_prs(&self, test_id: usize, start_nt: VarId, is_t_data: bool) -> ProdRuleSet<LL1> {
        let ll1 = self.try_build_prs(start_nt, is_t_data);
        assert_eq!(ll1.get_log().num_errors(), 0, "test {test_id}/{self:?}/{start_nt} failed:\n- {}", ll1.get_log().get_errors().join("\n- "));
        ll1
    }
}