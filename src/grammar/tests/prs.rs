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
        if  fail1 || fail2 || fail3 {
            errors += 1;
            if !show_answer_only {
                let msg = format!("## ERROR ## test {test_id}: ");
                if fail1 { println!("{msg}rules don't match"); }
                if fail2 { println!("{msg}flags don't match"); }
                if fail3 { println!("{msg}parents don't match"); }
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
            r#"a -> a_2"#,                                      // parent_+_or_*
            r#"a_1 -> B "," a_1 | ε"#,                          // child_+_or_*
            r#"a_2 -> A a_1 ";" a_2 | ε"#,                      // child_+_or_* | parent_+_or_*
        ], vec![2048, 1, 2049], vec![None, Some(2), Some(0)]),
        (150, vec![
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_1 | B a_1 | ε"#,                      // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (208, vec![
            r#"a -> i"#,                                        // parent_+_or_*
            r#"i -> A j ";" i | ε"#,                            // child_+_or_* | L-form | parent_+_or_*
            r#"j -> B "," j | ε"#,                              // child_+_or_* | L-form
        ], vec![2048, 2177, 129], vec![None, Some(0), Some(1)]),
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
            // e -> e "+" | "-" e | Num
            r#"e -> "-" e | Num e_1"#,                          // right_rec | parent_left_rec
            r#"e_1 -> "+" e_1 | ε"#,                            // child_left_rec
        ], vec![514, 4], vec![None, Some(0)]),
        (631, vec![
            // e -> e "+" | <R> "-" e | Num
            r#"e -> <R> "-" e | Num e_1"#,                      // right_rec | parent_left_rec
            r#"e_1 -> "+" e_1 | ε"#,                            // child_left_rec
        ], vec![514, 4], vec![None, Some(0)]),
        (632, vec![
            // e -> <R> e "+" | "-" e | Num
            r#"e -> "-" e | Num e_1"#,                          // right_rec | parent_left_rec
            r#"e_1 -> <R> "+" e_1 | ε"#,                        // child_left_rec
        ], vec![514, 4], vec![None, Some(0)]),

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
            // a -> (A (B ",")* ";")*
            r#"a -> a_2"#,                                      // parent_+_or_*
            r#"a_1 -> B "," a_1 | ε"#,                          // child_+_or_*
            r#"a_2 -> A a_1 ";" a_2 | ε"#,                      // child_+_or_* | parent_+_or_*
        ], vec![2048, 1, 2049], vec![None, Some(2), Some(0)]),
        (107, vec![
            // a -> (A (B ",")+ ";")+
            r#"a -> a_2"#,                                      // parent_+_or_* | plus
            r#"a_1 -> B "," a_3"#,                              // child_+_or_* | parent_left_fact | plus
            r#"a_2 -> A a_1 ";" a_4"#,                          // child_+_or_* | parent_left_fact | parent_+_or_* | plus
            r#"a_3 -> a_1 | ε"#,                                // child_left_fact
            r#"a_4 -> a_2 | ε"#,                                // child_left_fact
        ], vec![6144, 4129, 6177, 64, 64], vec![None, Some(2), Some(0), Some(1), Some(2)]),
        (200, vec![
            // a -> (<L=i> A)*
            r#"a -> i"#,                                        // parent_+_or_*
            r#"i -> A i | ε"#,                                  // child_+_or_* | L-form
        ], vec![2048, 129], vec![None, Some(0)]),
        (201, vec![
            // a -> (<L=i> A)+
            r#"a -> i"#,                                        // parent_+_or_* | plus
            r#"i -> A a_1"#,                                    // child_+_or_* | parent_left_fact | L-form | plus
            r#"a_1 -> i | ε"#,                                  // child_left_fact
        ], vec![6144, 4257, 64], vec![None, Some(0), Some(1)]),
        (208, vec![
            // a -> (<L=i> A (<L=j> B ",")* ";")*
            r#"a -> i"#,                                        // parent_+_or_*
            r#"i -> A j ";" i | ε"#,                            // child_+_or_* | L-form | parent_+_or_*
            r#"j -> B "," j | ε"#,                              // child_+_or_* | L-form
        ], vec![2048, 2177, 129], vec![None, Some(0), Some(1)]),
        (209, vec![
            // a -> (<L=i> A (<L=j> B ",")+ ";")+
            r#"a -> i"#,                                        // parent_+_or_* | plus
            r#"i -> A j ";" a_1"#,                              // child_+_or_* | parent_left_fact | L-form | parent_+_or_* | plus
            r#"j -> B "," a_2"#,                                // child_+_or_* | parent_left_fact | L-form | plus
            r#"a_1 -> i | ε"#,                                  // child_left_fact
            r#"a_2 -> j | ε"#,                                  // child_left_fact
        ], vec![6144, 6305, 4257, 64, 64], vec![None, Some(0), Some(1), Some(1), Some(2)]),
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
        (633, vec![
            // e -> e "*" e | <L=e> "!" e | e "+" e | Num
            r#"e -> e_4 e_1"#,                                  // L-form | parent_left_rec | parent_amb
            r#"e_1 -> "*" e_4 e_1 | "+" e_2 e_1 | ε"#,          // child_left_rec
            r#"e_2 -> e_4 e_3"#,                                // parent_left_rec
            r#"e_3 -> <G> "*" e_4 e_3 | ε"#,                    // child_left_rec
            r#"e_4 -> "!" e_2 | Num"#,                          // right_rec
        ], vec![1664, 4, 512, 4, 2], vec![None, Some(0), Some(0), Some(2), Some(0)]),
        (634, vec![
            // e -> e "+" | <L=e> "-" e | Num
            r#"e -> "-" e | Num e_1"#,                          // right_rec | L-form | parent_left_rec
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
            // a -> A B a | A C a | D
            r#"a -> A a_1 | D"#,                                // right_rec | parent_left_fact
            r#"a_1 -> B a | C a"#,                              // child_left_fact
        ], vec![34, 64], vec![None, Some(0)]),
        (851, vec![
            // a -> A (B | C) a <L=a> | D
            r#"a -> A a_1 | D"#,                                // right_rec | parent_left_fact | L-form
            r#"a_1 -> B a | C a"#,                              // child_left_fact
        ], vec![162, 64], vec![None, Some(0)]),
        (860, vec![
            // a -> a A B | a A C | D
            r#"a -> D a_1"#,                                    // parent_left_rec
            r#"a_1 -> A a_2 | ε"#,                              // child_left_rec | parent_left_fact
            r#"a_2 -> B a_1 | C a_1"#,                          // child_left_fact
        ], vec![512, 36, 64], vec![None, Some(0), Some(1)]),
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
        let rules = build_prs(test_id, false);
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
            "Not" => "Not",
            "Num" => "Num",
            "e" => "Not, Num",
            "e_1" => "Mul, Add, ε",
            "e_2" => "Not, Num",
            "e_3" => "Mul, ε",
            "e_4" => "Not, Num",
            "ε" => "ε",
        ]),
        (850, 0, hashmap![
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
        (850, 0, hashmap![
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
        (850, 0, false, vec![
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
        let result_is_ambiguous = !ll1.log.has_no_warnings();
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

#[cfg(any())]
#[test]
fn prs_calc_table_old() {
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