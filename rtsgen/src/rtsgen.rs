// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use std::collections::HashMap;
use std::io::Cursor;
use iter_index::IndexerIterator;
use vectree::VecTree;
use lexigram_lib::{CollectJoin, General, NameFixer, SymbolTable};
use lexigram_lib::dfa::TokenId;
use lexigram_lib::grammar::{GrNode, GrTree, RuleTreeSet, Symbol, VarId};
use lexigram_lib::io::CharReader;
use lexigram_lib::lexer::{Lexer, TokenSpliterator};
use lexigram_lib::log::{BufLog, LogStatus, Logger};
use lexigram_lib::parser::Parser;
use crate::rtsgen::listener_types::*;
use crate::rtsgen::rtsgen_lexer::build_lexer;
use crate::rtsgen::rtsgen_parser::*;

static T_GUESS_NAMES: &[(&str, &str); 47] = &[
    ("+", "Add"), ("-", "Sub"), ("*", "Mul"), ("/", "Div"), ("%", "Percent"), ("++", "Inc"), ("--", "Dec"),
    ("<<", "Shl"), (">>", "Shr"), ("!", "Not"), ("^", "Exp"), ("~", "Tilde"),
    ("&", "And"), ("|", "Or"), ("&&", "And2"), ("||", "Or2"),
    ("=", "Eq"), ("==", "Eq2"), ("<", "Lt"), (">", "Gt"), ("<=", "LtEq"), (">=", "GtEq"), ("!=", "Neq"),
    (":=", "ColonEq"), ("+=", "AddEq"), ("-=", "SubEq"), ("*=", "MulEq"), ("/=", "DivEq"),
    ("(", "LPar"), (")", "RPar"), ("[", "LSBracket"), ("]", "RSBracker"), ("{", "LBracket"), ("}", "RBracket"),
    ("\"", "DQuote"), ("'", "Quote"), ("$", "Dollar"), ("?", "Question"), ("\\", "Backslash"),
    (":", "Colon"), (";", "SemiColon"), (".", "Dot"), (",", "Comma"), ("#", "Sharp"), ("@", "At"), ("´", "Tick"), ("`", "BTick"),
];

pub struct RtsGen<'l, 'p> {
    lexer: Lexer<'l, Cursor<String>>,
    parser: Parser<'p>,
    guess_names: Option<HashMap<String, String>>,
}

impl RtsGen<'_, '_> {
    pub fn new() -> Self {
        let guess_names = Some(HashMap::from_iter(T_GUESS_NAMES.into_iter().map(|(a, b)| (a.to_string(), b.to_string()))));
        RtsGen::with_guess_names(guess_names)
    }

    pub fn with_guess_names(guess_names: Option<HashMap<String, String>>) -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        RtsGen { lexer, parser, guess_names }
    }

    pub fn parse(&mut self, text: String) -> Result<RuleTreeSet<General>, BufLog> {
        const VERBOSE_WRAPPER: bool = false;

        let mut wrapper = Wrapper::new(RGListener::new(self.guess_names.as_ref()), VERBOSE_WRAPPER);
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
            panic!("unexpected channel {ch} while parsing a file, line {line} col {col}, \"{text}\"")
        );
        let _ = self.parser.parse_stream(&mut wrapper, tokens); // errors are written in the log, so we can dismiss the error here
        let listener = wrapper.give_listener();
        listener.make_rts()
    }
}

// listener implementation

struct RGListener<'a> {
    log: BufLog,
    nt: HashMap<String, VarId>,
    nt_def_order: Vec<VarId>,
    rules: Vec<GrTree>,
    symbol_table: Option<SymbolTable>,
    t: HashMap<String, TokenId>,
    /// terminal information; `(String, Option<String>)` = (token name, optional string if not variable)
    tokens: Vec<(String, Option<String>)>,
    curr: Option<GrTree>,
    curr_name: Option<String>,
    curr_nt: Option<VarId>,
    guess_names: Option<&'a HashMap<String, String>>,
}

enum IsNew { No, Yes }

trait ToVarId {
    fn to_var_id(self, panic_message: &str) -> VarId;
}

impl ToVarId for usize {
    fn to_var_id(self, panic_message: &str) -> VarId {
        if self < (VarId::MAX as usize) {
            self as VarId
        } else {
            panic!("{panic_message}")
        }
    }
}

impl<'a> RGListener<'a> {
    fn new(guess_names: Option<&'a HashMap<String, String>>) -> Self {
        RGListener {
            log: BufLog::new(),
            nt: HashMap::new(),
            nt_def_order: Vec::new(),
            rules: Vec::new(),
            symbol_table: None,
            t: HashMap::new(),
            tokens: Vec::new(),
            curr: None,
            curr_name: None,
            curr_nt: None,
            guess_names,
        }
    }

    pub fn make_rts(self) -> Result<RuleTreeSet<General>, BufLog> {
        let RGListener { log, rules, symbol_table, .. } = self;
        if log.has_no_errors() {
            let mut rts = RuleTreeSet::<General>::with_log(log);
            rts.set_symbol_table(symbol_table.unwrap());
            for (var, rule) in rules.into_iter().index::<VarId>() {
                rts.set_tree(var, rule);
            }
            Ok(rts)
        } else {
            Err(log)
        }
    }

    /// Gets the nonterminal ID corresponding to the name, creating it if
    /// it doesn't exist.
    fn get_or_create_nt(&mut self, name: String) -> VarId {
        let size = self.nt.len();
        let var = *self.nt.entry(name).or_insert_with(|| {
            self.rules.push(GrTree::new());
            size.to_var_id("too many nonterminals")
        });
        var
    }

    /// Gets the terminal ID corresponding to the name, creating it if
    /// it doesn't exist.
    ///
    /// Returns
    /// * `IsNew` = `Yes` if the terminal had to be created, `No` otherwise
    /// * `TokenId` = the terminal ID
    fn get_or_create_t(&mut self, name: String) -> (IsNew, TokenId) {
        let mut is_new = IsNew::No;
        let size = self.t.len();
        let tok = *self.t.entry(name).or_insert_with(|| {
            is_new = IsNew::Yes;
            size.to_var_id("too many terminals") // VarId = TokenId
        });
        (is_new, tok)
    }

    /// Finalizes the rules and creates the symbol table.
    fn finalize_ruleset(&mut self) {
        let mut nt_name = vec![String::new(); self.nt.len()];
        let mut dest = vec![0; self.nt.len()];
        self.nt_def_order.iter().enumerate().for_each(|(i, &nt)| dest[nt as usize] = i);
        for (name, var) in &self.nt {
            nt_name[dest[*var as usize]] = name.clone();
        }

        // self.log.add_note(format!("NT def order:  {}", self.nt_def_order.iter().join(", ")));
        // self.log.add_note(format!("reorder table: {}", dest.iter().join(", ")));
        // self.log.add_note(format!("nt: {}", self.nt.iter().map(|(name, var)| format!("{name:?}->NT({var})")).join(", ")));
        let undefined = self.nt.iter()
            .filter_map(|(name, &var)| if self.rules[var as usize].is_empty() { Some(name.to_string()) } else { None })
            .to_vec();
        if !undefined.is_empty() {
            self.log.add_error(format!("undefined nonterminals: {}", undefined.join(", ")));
            return;
        }

        // build symbol table
        let mut symtab = SymbolTable::new();
        symtab.extend_nonterminals(nt_name);
        let mut t_name = vec![(String::new(), None); self.tokens.len()];
        let mut namefixer = NameFixer::new_empty();
        for (tok, (name, _cst_maybe)) in self.tokens.iter().enumerate().filter(|(_, (name, _))| !name.is_empty()) {
            namefixer.add(name.clone());
            t_name[tok] = (name.clone(), None);
        }

        // put names to constant terminals
        for (tok, (_, cst_maybe)) in self.tokens.iter().enumerate().filter(|(_, (name, _))| name.is_empty()) {
            let guess = self.guess_names.and_then(|g| g.get(cst_maybe.as_ref().unwrap()).cloned())
                .unwrap_or_else(|| format!("Token{tok}"));
            let new_name = namefixer.get_unique_name(guess);
            t_name[tok] = (new_name, cst_maybe.clone());
        }
        symtab.extend_terminals(t_name);
        self.symbol_table = Some(symtab);

        // reorder nonterminal IDs by order of definition rather than order of appearance
        self.rules = self.nt_def_order.iter()
            .map(|nt| std::mem::take(self.rules.get_mut(*nt as usize).unwrap()))
            .collect();
        for rule in &mut self.rules {
            for mut node in rule.iter_depth_simple_mut() {
                match *node {
                    // GrNode::Symbol(Symbol::NT(ref mut old))
                    // | GrNode::LForm(ref mut old) => *old = *conv.get(old).unwrap(),
                    GrNode::Symbol(Symbol::NT(ref mut old))
                    | GrNode::LForm(ref mut old) => *old = dest[*old as usize] as VarId,
                    _ => {}
                }
            }
        }
    }
}

impl RtsGenListener for RGListener<'_> {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit(&mut self, _ruleset: SynRuleset) {
        self.finalize_ruleset();
    }

    fn exit_ruleset(&mut self, _ctx: CtxRuleset) -> SynRuleset {
        SynRuleset()
    }

    fn init_rule(&mut self) {
        self.curr_name = None;
        self.curr = Some(VecTree::new());
        self.curr_nt = None;
    }

    fn exit_rule(&mut self, ctx: CtxRule) -> SynRule {
        let (var, id) = match ctx {
            // rule -> rule_nt "->" prs_expr ";"
            CtxRule::Rule1 { rule_nt: SynRuleNt(var), prs_expr: SynPrsExpr(id_expr) }
            // rule -> rule_nt "=>" rts_expr ";"
            | CtxRule::Rule2 { rule_nt: SynRuleNt(var), rts_expr: SynRtsExpr(id_expr) } => (var, id_expr),
        };
        let mut tree = self.curr.take().unwrap();
        tree.set_root(id);
        self.rules[var as usize] = tree;
        SynRule()
    }

    fn exit_rule_nt(&mut self, ctx: CtxRuleNt) -> SynRuleNt {
        let CtxRuleNt::RuleNt { nonterminal } = ctx;
        assert_eq!(self.curr_nt, None);
        let var = self.get_or_create_nt(nonterminal.clone());
        self.nt_def_order.push(var);
        self.curr_nt = Some(var);
        self.curr_name = Some(nonterminal);
        SynRuleNt(var)
    }

    fn exit_rts_expr(&mut self, ctx: CtxRtsExpr) -> SynRtsExpr {
        let tree = self.curr.as_mut().unwrap();
        let id = match ctx {
            // rts_expr -> "&" rts_children
            CtxRtsExpr::RtsExpr1 { rts_children: SynRtsChildren(v) } =>
                tree.addci_iter(None, GrNode::Concat, v.into_iter().map(|SynRtsExpr(id)| id)),
            // rts_expr -> "|" rts_children
            CtxRtsExpr::RtsExpr2 { rts_children: SynRtsChildren(v) } =>
                tree.addci_iter(None, GrNode::Or, v.into_iter().map(|SynRtsExpr(id)| id)),
            // rts_expr -> "+" rts_children
            CtxRtsExpr::RtsExpr3 { rts_children: SynRtsChildren(v) } =>
                tree.addci_iter(None, GrNode::Plus, v.into_iter().map(|SynRtsExpr(id)| id)),
            // rts_expr -> "*" rts_children
            CtxRtsExpr::RtsExpr4 { rts_children: SynRtsChildren(v) } =>
                tree.addci_iter(None, GrNode::Star, v.into_iter().map(|SynRtsExpr(id)| id)),
            // rts_expr -> "?" rts_children
            CtxRtsExpr::RtsExpr5 { rts_children: SynRtsChildren(v) } =>
                tree.addci_iter(None, GrNode::Maybe, v.into_iter().map(|SynRtsExpr(id)| id)),
            // rts_expr -> item
            CtxRtsExpr::RtsExpr6 { item: SynItem(id_item) } =>
                id_item,
        };
        SynRtsExpr(id)
    }

    fn exit_rts_children(&mut self, ctx: CtxRtsChildren) -> SynRtsChildren {
        // rts_children -> "(" rts_expr* ")"
        let CtxRtsChildren::RtsChildren { star: SynRtsChildren1(v) } = ctx;
        SynRtsChildren(v)
    }

    fn exit_prs_expr(&mut self, ctx: CtxPrsExpr) -> SynPrsExpr {
        let tree = self.curr.as_mut().unwrap();
        let id = match ctx {
            // prs_expr -> prs_expr "+"
            CtxPrsExpr::PrsExpr1 { prs_expr: SynPrsExpr(id) } =>
                tree.addci(None, GrNode::Plus, id),
            // prs_expr -> prs_expr "*"
            CtxPrsExpr::PrsExpr2 { prs_expr: SynPrsExpr(id) } =>
                tree.addci(None, GrNode::Star, id),
            // prs_expr -> prs_expr "?"
            CtxPrsExpr::PrsExpr3 { prs_expr: SynPrsExpr(id) } =>
                tree.addci(None, GrNode::Maybe, id),
            // prs_expr -> prs_expr prs_expr
            CtxPrsExpr::PrsExpr4 { prs_expr: [SynPrsExpr(mut left), SynPrsExpr(right)] } => {
                if *tree.get(left) != GrNode::Concat {
                    left = tree.addci(None, GrNode::Concat, left);
                }
                tree.attach_child(left, right);
                left
            }
            // prs_expr -> prs_expr "|" prs_expr
            CtxPrsExpr::PrsExpr5 { prs_expr: [SynPrsExpr(mut left), SynPrsExpr(right)] } => {
                if *tree.get(left) != GrNode::Or {
                    left = tree.addci(None, GrNode::Or, left);
                }
                tree.attach_child(left, right);
                left
            }
            // prs_expr -> "(" prs_expr ")"
            CtxPrsExpr::PrsExpr6 { prs_expr: SynPrsExpr(id) } => id,
            // prs_expr -> item
            CtxPrsExpr::PrsExpr7 { item: SynItem(id) } => id,
        };
        SynPrsExpr(id)
    }

    fn exit_item(&mut self, ctx: CtxItem) -> SynItem {
        let id = match ctx {
            // item -> Nonterminal
            CtxItem::Item1 { nonterminal } => {
                let var = self.get_or_create_nt(nonterminal);
                self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::NT(var)))
            }
            // item -> Terminal
            CtxItem::Item2 { terminal } => {
                let (is_new, tok) = self.get_or_create_t(terminal.clone());
                if let IsNew::Yes = is_new {
                    self.tokens.push((terminal, None));
                }
                self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::T(tok)))
            }
            // item -> TerminalCst
            CtxItem::Item3 { terminalcst } => {
                let (is_new, tok) = self.get_or_create_t(terminalcst.clone());
                if let IsNew::Yes = is_new {
                    // the names will be set later, to give priority to variable terminal names in case of conflict
                    match decode_str(&terminalcst[1..terminalcst.len() - 1]) {
                        Ok(text) => {
                            self.tokens.push((String::new(), Some(text)));
                        }
                        Err(msg) => {
                            self.log.add_error(format!("in {}, string literal {terminalcst}: {msg}", self.curr_name.as_ref().unwrap()));
                            self.tokens.push((String::new(), Some("???".to_string())));
                        }
                    }
                }
                self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::T(tok)))
            }
            // item -> Empty
            CtxItem::Item4 { .. } =>
                self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::Empty)),
            // item -> LTag
            CtxItem::Item5 { ltag } => {
                // `ltag` contains "<L=name>" or "<L>"
                let var = if ltag.len() > 3 {
                    let name = &ltag[3..ltag.len()-1];
                    let var = self.get_or_create_nt(name.to_string());
                    self.nt_def_order.push(var);
                    self.rules[var as usize].add_root(GrNode::Symbol(Symbol::Empty));
                    var
                } else {
                    self.curr_nt.unwrap()
                };
                self.curr.as_mut().unwrap().add(None, GrNode::LForm(var))
            }
            // item -> "<P>"
            CtxItem::Item6 => self.curr.as_mut().unwrap().add(None, GrNode::PrecEq),
            // item -> "<R>"
            CtxItem::Item7 => self.curr.as_mut().unwrap().add(None, GrNode::RAssoc),
        };
        SynItem(id)
    }
}

/// Decodes a string literal (without its surrounding quotes). There must be at least two characters in `strlit`.
fn decode_str(strlit: &str) -> Result<String, String> {
    let mut result = String::new();
    let mut chars = strlit.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                result.push(match chars.next().ok_or(format!("'\\' incomplete escape code in string literal '{strlit}'"))? {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\"' => '\"',
                    '\\' => '\\',
                    'u' => {
                        if !matches!(chars.next(), Some('{')) { return Err(format!("malformed unicode literal in string literal '{strlit}' (missing '{{')")); }
                        let mut hex = String::new();
                        loop {
                            let Some(h) = chars.next() else { return Err(format!("malformed unicode literal in string literal '{strlit}' (missing '}}')")); };
                            if h == '}' { break; }
                            hex.push(h);
                        };
                        let code = u32::from_str_radix(&hex, 16).map_err(|_| format!("'{hex}' isn't a valid hexadecimal value"))?;
                        char::from_u32(code).ok_or_else(|| format!("'{hex}' isn't a valid unicode hexadecimal value"))?
                    }
                    unknown => return Err(format!("unknown escape code '\\{unknown}' in string literal '{strlit}'"))
                });
            }
            _ => result.push(c)
        }
    }
    Ok(result)
}

// -------------------------------------------------------------------------
// User types used in the listener interface:
// (initially copied/uncommented from the generated parser code)

pub mod listener_types {
    use lexigram_lib::grammar::VarId;

    /// User-defined type for `ruleset`
    #[derive(Debug, PartialEq)] pub struct SynRuleset();
    /// User-defined type for `rule`
    #[derive(Debug, PartialEq)] pub struct SynRule();
    /// User-defined type for `rule_nt`
    #[derive(Debug, PartialEq)] pub struct SynRuleNt(pub VarId);
    /// User-defined type for `rts_expr`
    #[derive(Debug, PartialEq)] pub struct SynRtsExpr(pub usize);
    /// User-defined type for `rts_children`
    #[derive(Debug, PartialEq)] pub struct SynRtsChildren(pub Vec<SynRtsExpr>);
    /// User-defined type for `prs_expr`
    #[derive(Debug, PartialEq)] pub struct SynPrsExpr(pub usize);
    /// User-defined type for `item`
    #[derive(Debug, PartialEq)] pub struct SynItem(pub usize);
}

// -------------------------------------------------------------------------

pub mod rtsgen_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [rtsgen_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_lib::dfa::{StateId, Terminal, ActionOption, ModeOption};
    use lexigram_lib::lexer::Lexer;
    use lexigram_lib::lexergen::GroupId;
    use lexigram_lib::segments::{Seg, SegMap};

    const NBR_GROUPS: u32 = 33;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 19;
    const NBR_STATES: StateId = 39;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         26,  26,  26,  26,  26,  26,  26,  26,  26,  18,  32,  26,  26,  32,  26,  26,   // 0-15
         26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,   // 16-31
          0,  26,   1,  26,  26,  26,   2,  26,   3,   4,   5,   6,  26,   7,  26,   8,   // 32-47
         25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  26,   9,  10,  11,  19,  12,   // 48-63
         26,  24,  24,  24,  24,  24,  24,  29,  29,  29,  29,  29,  13,  29,  29,  29,   // 64-79
         22,  29,  23,  29,  29,  29,  29,  29,  29,  29,  29,  26,  20,  26,  26,  30,   // 80-95
         26,  28,  28,  28,  28,  28,  28,  31,  31,  31,  31,  31,  31,  31,  14,  31,   // 96-111
         31,  31,  14,  31,  14,  27,  31,  31,  31,  31,  31,  15,  16,  21,  26,  26,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 2] = [
        ('ε', 17),
        ('€', 17),
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 4] = [
        (Seg(128, 948), 26),
        (Seg(950, 8363), 26),
        (Seg(8365, 55295), 26),
        (Seg(57344, 1114111), 26),
    ];
    static TERMINAL_TABLE: [Terminal;20] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 1288] = [
         19,   1,  20,  21,  22,  23,  24,   2,   3,  25,   4,   5,  26,  27,  28,   6,  29,  30,  19,  39,  39,  39,  27,  27,  27,  39,  39,  28,  28,  27,  39,  28,  19, // state 0
         14,  39,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  39,  14,  15,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  39, // state 1
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  33,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 2
         39,  39,  39,  39,  39,   7,  39,  39,  31,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 3
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,   9,  39,  39,  39,  39,  39,  39,  39,  39,  10,  11,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 4
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  34,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 5
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  30,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 6
          7,   7,   7,   7,   7,   8,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 7
          7,   7,   7,   7,   7,   8,   7,   7,  32,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 8
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  12,  39,  39,  39,  39,  39,  39,  39,  35,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 9
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  36,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 10
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  37,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 11
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  13,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  13,  13,  39,  39,  13,  39, // state 12
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  13,  39,  39,  39,  39,  35,  39,  39,  39,  39,  39,  13,  39,  13,  13,  39,  13,  13,  39, // state 13
         14,  38,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  39,  14,  15,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  39, // state 14
         39,  14,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  14,  39,  39,  39,  39,  39,  14,  39,  39,  39,  39,  39,  39,  16,  39,  39,  39,  39,  39, // state 15
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  17,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 16
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  18,  18,  39,  39,  18,  39,  39,  39,  39, // state 17
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  14,  39,  39,  18,  18,  39,  39,  18,  39,  39,  39,  39, // state 18
         19,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  19,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  19, // state 19 <skip>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 20 <end:2>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 21 <end:8>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 22 <end:9>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 23 <end:5>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 24 <end:4>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 25 <end:10>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 26 <end:6>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  27,  27,  39,  39,  39,  39,  39,  39,  39,  27,  27,  27,  27,  39,  27,  27,  27,  27,  27,  39, // state 27 <end:15>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  28,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  28,  39,  28,  28,  39,  28,  28,  39, // state 28 <end:16>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 29 <end:3>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 30 <end:7>
         31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  39, // state 31 <skip>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 32 <skip>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 33 <end:0>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 34 <end:1>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 35 <end:11>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 36 <end:12>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 37 <end:13>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39, // state 38 <end:14>
         39 // error group in [nbr_state * nbr_group + nbr_group]
    ];

    pub fn build_lexer<R: Read>() -> Lexer<'static, R> {
        Lexer::new(
            // parameters
            NBR_GROUPS,
            INITIAL_STATE,
            FIRST_END_STATE,
            NBR_STATES,
            // tables
            &ASCII_TO_GROUP,
            HashMap::<char, GroupId>::from(UTF8_TO_GROUP),
            SegMap::<GroupId>::from(SEG_TO_GROUP),
            &STATE_TABLE,
            &TERMINAL_TABLE,
        )
    }

    // [rtsgen_lexer]
}

// -------------------------------------------------------------------------

pub mod rtsgen_parser {
    // Generated code, don't modify manually anything between the tags below

    // [rtsgen_parser]

    use lexigram_lib::{CollectJoin, FixedSymTable, grammar::{AltId, Alternative, Symbol, VarId}, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 17;
    const PARSER_NUM_NT: usize = 17;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Arrow", Some("->")), ("DArrow", Some("=>")), ("Concat", Some("&")), ("Or", Some("|")), ("Plus", Some("+")), ("Star", Some("*")), ("Question", Some("?")), ("Empty", None), ("LPar", Some("(")), ("RPar", Some(")")), ("Semicolon", Some(";")), ("LTag", None), ("PTag", Some("<P>")), ("RTag", Some("<R>")), ("TerminalCst", None), ("Terminal", None), ("Nonterminal", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["ruleset", "rule_iter", "rule", "rule_nt", "rts_expr", "rts_children", "prs_expr", "item", "rts_children_1", "prs_expr_1", "prs_expr_2", "prs_expr_3", "prs_expr_4", "prs_expr_5", "prs_expr_6", "ruleset_1", "rule_1"];
    static ALT_VAR: [VarId; 44] = [0, 1, 2, 3, 4, 4, 4, 4, 4, 4, 5, 6, 7, 7, 7, 7, 7, 7, 7, 8, 8, 9, 9, 9, 9, 9, 9, 10, 11, 11, 11, 11, 11, 12, 13, 13, 13, 13, 14, 14, 15, 15, 16, 16];
    static ALTERNATIVES: [&[Symbol]; 44] = [&[Symbol::NT(1)], &[Symbol::NT(2), Symbol::NT(15)], &[Symbol::NT(3), Symbol::NT(16)], &[Symbol::T(16)], &[Symbol::T(2), Symbol::NT(5)], &[Symbol::T(3), Symbol::NT(5)], &[Symbol::T(4), Symbol::NT(5)], &[Symbol::T(5), Symbol::NT(5)], &[Symbol::T(6), Symbol::NT(5)], &[Symbol::NT(7)], &[Symbol::T(8), Symbol::NT(8), Symbol::T(9)], &[Symbol::NT(14), Symbol::NT(9)], &[Symbol::T(16)], &[Symbol::T(15)], &[Symbol::T(14)], &[Symbol::T(7)], &[Symbol::T(11)], &[Symbol::T(12)], &[Symbol::T(13)], &[Symbol::NT(4), Symbol::NT(8)], &[Symbol::Empty], &[Symbol::T(4), Symbol::NT(9)], &[Symbol::T(5), Symbol::NT(9)], &[Symbol::T(6), Symbol::NT(9)], &[Symbol::NT(12), Symbol::NT(9)], &[Symbol::T(3), Symbol::NT(10), Symbol::NT(9)], &[Symbol::Empty], &[Symbol::NT(14), Symbol::NT(11)], &[Symbol::T(4), Symbol::NT(11)], &[Symbol::T(5), Symbol::NT(11)], &[Symbol::T(6), Symbol::NT(11)], &[Symbol::NT(12), Symbol::NT(11)], &[Symbol::Empty], &[Symbol::NT(14), Symbol::NT(13)], &[Symbol::T(4), Symbol::NT(13)], &[Symbol::T(5), Symbol::NT(13)], &[Symbol::T(6), Symbol::NT(13)], &[Symbol::Empty], &[Symbol::T(8), Symbol::NT(6), Symbol::T(9)], &[Symbol::NT(7)], &[Symbol::NT(1)], &[Symbol::Empty], &[Symbol::T(0), Symbol::NT(6), Symbol::T(10)], &[Symbol::T(1), Symbol::NT(4), Symbol::T(10)]];
    static PARSING_TABLE: [AltId; 306] = [44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 0, 45, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 1, 45, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 2, 45, 45, 45, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 3, 44, 44, 44, 4, 5, 6, 7, 8, 9, 44, 45, 45, 9, 9, 9, 9, 9, 9, 44, 44, 44, 45, 45, 45, 45, 45, 45, 10, 45, 45, 45, 45, 45, 45, 45, 45, 44, 44, 44, 44, 44, 44, 44, 44, 11, 11, 45, 45, 11, 11, 11, 11, 11, 11, 44, 44, 44, 45, 45, 45, 45, 45, 15, 45, 45, 45, 16, 17, 18, 14, 13, 12, 44, 44, 44, 19, 19, 19, 19, 19, 19, 44, 20, 44, 19, 19, 19, 19, 19, 19, 44, 44, 44, 44, 25, 21, 22, 23, 24, 24, 26, 26, 24, 24, 24, 24, 24, 24, 44, 44, 44, 44, 45, 45, 45, 45, 27, 27, 45, 45, 27, 27, 27, 27, 27, 27, 44, 44, 44, 44, 32, 28, 29, 30, 31, 31, 32, 32, 31, 31, 31, 31, 31, 31, 44, 44, 44, 44, 45, 45, 45, 45, 33, 33, 45, 45, 33, 33, 33, 33, 33, 33, 44, 44, 44, 44, 37, 34, 35, 36, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 44, 44, 44, 44, 45, 45, 45, 45, 39, 38, 45, 45, 39, 39, 39, 39, 39, 39, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 40, 41, 42, 43, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 45, 45];
    static OPCODES: [&[OpCode]; 44] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::NT(15), OpCode::NT(2)], &[OpCode::NT(16), OpCode::NT(3)], &[OpCode::Exit(3), OpCode::T(16)], &[OpCode::Exit(4), OpCode::NT(5), OpCode::T(2)], &[OpCode::Exit(5), OpCode::NT(5), OpCode::T(3)], &[OpCode::Exit(6), OpCode::NT(5), OpCode::T(4)], &[OpCode::Exit(7), OpCode::NT(5), OpCode::T(5)], &[OpCode::Exit(8), OpCode::NT(5), OpCode::T(6)], &[OpCode::Exit(9), OpCode::NT(7)], &[OpCode::Exit(10), OpCode::T(9), OpCode::NT(8), OpCode::T(8)], &[OpCode::NT(9), OpCode::Exit(11), OpCode::NT(14)], &[OpCode::Exit(12), OpCode::T(16)], &[OpCode::Exit(13), OpCode::T(15)], &[OpCode::Exit(14), OpCode::T(14)], &[OpCode::Exit(15), OpCode::T(7)], &[OpCode::Exit(16), OpCode::T(11)], &[OpCode::Exit(17), OpCode::T(12)], &[OpCode::Exit(18), OpCode::T(13)], &[OpCode::Loop(8), OpCode::Exit(19), OpCode::NT(4)], &[OpCode::Exit(20)], &[OpCode::Loop(9), OpCode::Exit(21), OpCode::T(4)], &[OpCode::Loop(9), OpCode::Exit(22), OpCode::T(5)], &[OpCode::Loop(9), OpCode::Exit(23), OpCode::T(6)], &[OpCode::Loop(9), OpCode::Exit(24), OpCode::NT(12)], &[OpCode::Loop(9), OpCode::Exit(25), OpCode::NT(10), OpCode::T(3)], &[OpCode::Exit(26)], &[OpCode::NT(11), OpCode::Exit(27), OpCode::NT(14)], &[OpCode::Loop(11), OpCode::Exit(28), OpCode::T(4)], &[OpCode::Loop(11), OpCode::Exit(29), OpCode::T(5)], &[OpCode::Loop(11), OpCode::Exit(30), OpCode::T(6)], &[OpCode::Loop(11), OpCode::Exit(31), OpCode::NT(12)], &[OpCode::Exit(32)], &[OpCode::NT(13), OpCode::Exit(33), OpCode::NT(14)], &[OpCode::Loop(13), OpCode::Exit(34), OpCode::T(4)], &[OpCode::Loop(13), OpCode::Exit(35), OpCode::T(5)], &[OpCode::Loop(13), OpCode::Exit(36), OpCode::T(6)], &[OpCode::Exit(37)], &[OpCode::Exit(38), OpCode::T(9), OpCode::NT(6), OpCode::T(8)], &[OpCode::Exit(39), OpCode::NT(7)], &[OpCode::Loop(1), OpCode::Exit(40)], &[OpCode::Exit(41)], &[OpCode::Exit(42), OpCode::T(10), OpCode::NT(6), OpCode::T(0)], &[OpCode::Exit(43), OpCode::T(10), OpCode::NT(4), OpCode::T(1)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser<'static> {
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        Parser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }

    #[derive(Debug)]
    pub enum CtxRuleset {
        /// `ruleset -> (<L> rule)+`
        Ruleset,
    }
    #[derive(Debug)]
    pub enum CtxRuleIter {
        /// `ruleset -> <L> rule`
        RuleIter { rule: SynRule, last_iteration: bool },
    }
    #[derive(Debug)]
    pub enum CtxRule {
        /// `rule -> rule_nt "->" prs_expr ";"`
        Rule1 { rule_nt: SynRuleNt, prs_expr: SynPrsExpr },
        /// `rule -> rule_nt "=>" rts_expr ";"`
        Rule2 { rule_nt: SynRuleNt, rts_expr: SynRtsExpr },
    }
    #[derive(Debug)]
    pub enum CtxRuleNt {
        /// `rule_nt -> Nonterminal`
        RuleNt { nonterminal: String },
    }
    #[derive(Debug)]
    pub enum CtxRtsExpr {
        /// `rts_expr -> "&" rts_children`
        RtsExpr1 { rts_children: SynRtsChildren },
        /// `rts_expr -> "|" rts_children`
        RtsExpr2 { rts_children: SynRtsChildren },
        /// `rts_expr -> "+" rts_children`
        RtsExpr3 { rts_children: SynRtsChildren },
        /// `rts_expr -> "*" rts_children`
        RtsExpr4 { rts_children: SynRtsChildren },
        /// `rts_expr -> "?" rts_children`
        RtsExpr5 { rts_children: SynRtsChildren },
        /// `rts_expr -> item`
        RtsExpr6 { item: SynItem },
    }
    #[derive(Debug)]
    pub enum CtxRtsChildren {
        /// `rts_children -> "(" rts_expr* ")"`
        RtsChildren { star: SynRtsChildren1 },
    }
    #[derive(Debug)]
    pub enum CtxPrsExpr {
        /// `prs_expr -> prs_expr "+"`
        PrsExpr1 { prs_expr: SynPrsExpr },
        /// `prs_expr -> prs_expr "*"`
        PrsExpr2 { prs_expr: SynPrsExpr },
        /// `prs_expr -> prs_expr "?"`
        PrsExpr3 { prs_expr: SynPrsExpr },
        /// `prs_expr -> prs_expr prs_expr`
        PrsExpr4 { prs_expr: [SynPrsExpr; 2] },
        /// `prs_expr -> prs_expr "|" prs_expr`
        PrsExpr5 { prs_expr: [SynPrsExpr; 2] },
        /// `prs_expr -> "(" prs_expr ")"`
        PrsExpr6 { prs_expr: SynPrsExpr },
        /// `prs_expr -> item`
        PrsExpr7 { item: SynItem },
    }
    #[derive(Debug)]
    pub enum CtxItem {
        /// `item -> Nonterminal`
        Item1 { nonterminal: String },
        /// `item -> Terminal`
        Item2 { terminal: String },
        /// `item -> TerminalCst`
        Item3 { terminalcst: String },
        /// `item -> Empty`
        Item4 { empty: String },
        /// `item -> LTag`
        Item5 { ltag: String },
        /// `item -> "<P>"`
        Item6,
        /// `item -> "<R>"`
        Item7,
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `ruleset`
    // #[derive(Debug, PartialEq)] pub struct SynRuleset();
    // /// User-defined type for `rule`
    // #[derive(Debug, PartialEq)] pub struct SynRule();
    // /// User-defined type for `rule_nt`
    // #[derive(Debug, PartialEq)] pub struct SynRuleNt();
    // /// User-defined type for `rts_expr`
    // #[derive(Debug, PartialEq)] pub struct SynRtsExpr();
    // /// User-defined type for `rts_children`
    // #[derive(Debug, PartialEq)] pub struct SynRtsChildren();
    // /// User-defined type for `prs_expr`
    // #[derive(Debug, PartialEq)] pub struct SynPrsExpr();
    // /// User-defined type for `item`
    // #[derive(Debug, PartialEq)] pub struct SynItem();
    /// Computed `rts_expr*` array in `rts_children -> "("  ►► rts_expr* ◄◄  ")"`
    #[derive(Debug, PartialEq)]
    pub struct SynRtsChildren1(pub Vec<SynRtsExpr>);

    #[derive(Debug)]
    enum SynValue { Ruleset(SynRuleset), Rule(SynRule), RuleNt(SynRuleNt), RtsExpr(SynRtsExpr), RtsChildren(SynRtsChildren), PrsExpr(SynPrsExpr), Item(SynItem), RtsChildren1(SynRtsChildren1) }

    impl SynValue {
        fn get_ruleset(self) -> SynRuleset {
            if let SynValue::Ruleset(val) = self { val } else { panic!() }
        }
        fn get_rule(self) -> SynRule {
            if let SynValue::Rule(val) = self { val } else { panic!() }
        }
        fn get_rule_nt(self) -> SynRuleNt {
            if let SynValue::RuleNt(val) = self { val } else { panic!() }
        }
        fn get_rts_expr(self) -> SynRtsExpr {
            if let SynValue::RtsExpr(val) = self { val } else { panic!() }
        }
        fn get_rts_children(self) -> SynRtsChildren {
            if let SynValue::RtsChildren(val) = self { val } else { panic!() }
        }
        fn get_prs_expr(self) -> SynPrsExpr {
            if let SynValue::PrsExpr(val) = self { val } else { panic!() }
        }
        fn get_item(self) -> SynItem {
            if let SynValue::Item(val) = self { val } else { panic!() }
        }
        fn get_rts_children1(self) -> SynRtsChildren1 {
            if let SynValue::RtsChildren1(val) = self { val } else { panic!() }
        }
    }

    pub trait RtsGenListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> bool { false }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        fn exit(&mut self, _ruleset: SynRuleset) {}
        fn init_ruleset(&mut self) {}
        fn exit_ruleset(&mut self, _ctx: CtxRuleset) -> SynRuleset;
        fn init_rule_iter(&mut self) {}
        fn exit_rule_iter(&mut self, _ctx: CtxRuleIter) {}
        fn init_rule(&mut self) {}
        fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule;
        fn init_rule_nt(&mut self) {}
        fn exit_rule_nt(&mut self, _ctx: CtxRuleNt) -> SynRuleNt;
        fn init_rts_expr(&mut self) {}
        fn exit_rts_expr(&mut self, _ctx: CtxRtsExpr) -> SynRtsExpr;
        fn init_rts_children(&mut self) {}
        fn exit_rts_children(&mut self, _ctx: CtxRtsChildren) -> SynRtsChildren;
        fn init_prs_expr(&mut self) {}
        fn exit_prs_expr(&mut self, _ctx: CtxPrsExpr) -> SynPrsExpr;
        fn init_item(&mut self) {}
        fn exit_item(&mut self, _ctx: CtxItem) -> SynItem;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: RtsGenListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_ruleset(),          // ruleset
                        1 => self.listener.init_rule_iter(),        // rule_iter
                        15 => {}                                    // ruleset_1
                        2 => self.listener.init_rule(),             // rule
                        16 => {}                                    // rule_1
                        3 => self.listener.init_rule_nt(),          // rule_nt
                        4 => self.listener.init_rts_expr(),         // rts_expr
                        5 => self.listener.init_rts_children(),     // rts_children
                        8 => self.init_rts_children1(),             // rts_children_1
                        6 => self.listener.init_prs_expr(),         // prs_expr
                        9 ..= 14 => {}                              // prs_expr_1, prs_expr_2, prs_expr_3, prs_expr_4, prs_expr_5, prs_expr_6
                        7 => self.listener.init_item(),             // item
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_ruleset(),                   // ruleset -> rule_iter
                        40 |                                        // ruleset_1 -> rule_iter
                        41 => self.exit_rule_iter(alt_id),          // ruleset_1 -> ε
                     /* 1 */                                        // rule_iter -> <L> rule ruleset_1 (never called)
                        42 |                                        // rule_1 -> -> prs_expr ;
                        43 => self.exit_rule(alt_id),               // rule_1 -> => rts_expr ;
                     /* 2 */                                        // rule -> rule_nt rule_1 (never called)
                        3 => self.exit_rule_nt(),                   // rule_nt -> Nonterminal
                        4 |                                         // rts_expr -> & rts_children
                        5 |                                         // rts_expr -> | rts_children
                        6 |                                         // rts_expr -> + rts_children
                        7 |                                         // rts_expr -> * rts_children
                        8 |                                         // rts_expr -> ? rts_children
                        9 => self.exit_rts_expr(alt_id),            // rts_expr -> item
                        10 => self.exit_rts_children(),             // rts_children -> ( rts_children_1 )
                        19 => self.exit_rts_children1(),            // rts_children_1 -> rts_expr rts_children_1
                        20 => {}                                    // rts_children_1 -> ε
                        21 |                                        // prs_expr_1 -> + prs_expr_1
                        22 |                                        // prs_expr_1 -> * prs_expr_1
                        23 |                                        // prs_expr_1 -> ? prs_expr_1
                        24 |                                        // prs_expr_1 -> prs_expr_4 prs_expr_1
                        25 => self.exit_prs_expr1(alt_id),          // prs_expr_1 -> | prs_expr_2 prs_expr_1
                        28 |                                        // prs_expr_3 -> + prs_expr_3 (duplicate of 21)
                        34 => self.exit_prs_expr1(21),              // prs_expr_5 -> + prs_expr_5 (duplicate of 21)
                        29 |                                        // prs_expr_3 -> * prs_expr_3 (duplicate of 22)
                        35 => self.exit_prs_expr1(22),              // prs_expr_5 -> * prs_expr_5 (duplicate of 22)
                        30 |                                        // prs_expr_3 -> ? prs_expr_3 (duplicate of 23)
                        36 => self.exit_prs_expr1(23),              // prs_expr_5 -> ? prs_expr_5 (duplicate of 23)
                        31 => self.exit_prs_expr1(24),              // prs_expr_3 -> prs_expr_4 prs_expr_3 (duplicate of 24)
                        38 |                                        // prs_expr_6 -> ( prs_expr )
                        39 => self.exit_prs_expr6(alt_id),          // prs_expr_6 -> item
                        11 => {}                                    // prs_expr -> prs_expr_6 prs_expr_1 (not used)
                        26 => {}                                    // prs_expr_1 -> ε (not used)
                        27 => {}                                    // prs_expr_2 -> prs_expr_6 prs_expr_3 (not used)
                        32 => {}                                    // prs_expr_3 -> ε (not used)
                        33 => {}                                    // prs_expr_4 -> prs_expr_6 prs_expr_5 (not used)
                        37 => {}                                    // prs_expr_5 -> ε (not used)
                        12 |                                        // item -> Nonterminal
                        13 |                                        // item -> Terminal
                        14 |                                        // item -> TerminalCst
                        15 |                                        // item -> Empty
                        16 |                                        // item -> LTag
                        17 |                                        // item -> <P>
                        18 => self.exit_item(alt_id),               // item -> <R>
                        _ => panic!("unexpected exit alternative id: {alt_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }

        fn check_abort_request(&self) -> bool {
            self.listener.check_abort_request()
        }

        fn get_mut_log(&mut self) -> &mut impl Logger {
            self.listener.get_mut_log()
        }
    }

    impl<T: RtsGenListener> Wrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn get_listener(&self) -> &T {
            &self.listener
        }

        pub fn get_listener_mut(&mut self) -> &mut T {
            &mut self.listener
        }

        pub fn give_listener(self) -> T {
            self.listener
        }

        pub fn set_verbose(&mut self, verbose: bool) {
            self.verbose = verbose;
        }

        fn exit(&mut self) {
            let ruleset = self.stack.pop().unwrap().get_ruleset();
            self.listener.exit(ruleset);
        }

        fn exit_ruleset(&mut self) {
            let val = self.listener.exit_ruleset(CtxRuleset::Ruleset);
            self.stack.push(SynValue::Ruleset(val));
        }

        fn exit_rule_iter(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 41;
            let rule = self.stack.pop().unwrap().get_rule();
            self.listener.exit_rule_iter(CtxRuleIter::RuleIter { rule, last_iteration });
        }

        fn exit_rule(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                42 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    let rule_nt = self.stack.pop().unwrap().get_rule_nt();
                    CtxRule::Rule1 { rule_nt, prs_expr }
                }
                43 => {
                    let rts_expr = self.stack.pop().unwrap().get_rts_expr();
                    let rule_nt = self.stack.pop().unwrap().get_rule_nt();
                    CtxRule::Rule2 { rule_nt, rts_expr }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_rule")
            };
            let val = self.listener.exit_rule(ctx);
            self.stack.push(SynValue::Rule(val));
        }

        fn exit_rule_nt(&mut self) {
            let nonterminal = self.stack_t.pop().unwrap();
            let val = self.listener.exit_rule_nt(CtxRuleNt::RuleNt { nonterminal });
            self.stack.push(SynValue::RuleNt(val));
        }

        fn exit_rts_expr(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                4 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr1 { rts_children }
                }
                5 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr2 { rts_children }
                }
                6 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr3 { rts_children }
                }
                7 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr4 { rts_children }
                }
                8 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr5 { rts_children }
                }
                9 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRtsExpr::RtsExpr6 { item }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_rts_expr")
            };
            let val = self.listener.exit_rts_expr(ctx);
            self.stack.push(SynValue::RtsExpr(val));
        }

        fn exit_rts_children(&mut self) {
            let star = self.stack.pop().unwrap().get_rts_children1();
            let val = self.listener.exit_rts_children(CtxRtsChildren::RtsChildren { star });
            self.stack.push(SynValue::RtsChildren(val));
        }

        fn init_rts_children1(&mut self) {
            let val = SynRtsChildren1(Vec::new());
            self.stack.push(SynValue::RtsChildren1(val));
        }

        fn exit_rts_children1(&mut self) {
            let rts_expr = self.stack.pop().unwrap().get_rts_expr();
            let mut star_it = self.stack.pop().unwrap().get_rts_children1();
            star_it.0.push(rts_expr);
            self.stack.push(SynValue::RtsChildren1(star_it));
        }

        fn exit_prs_expr1(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                21 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr1 { prs_expr }
                }
                22 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr2 { prs_expr }
                }
                23 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr3 { prs_expr }
                }
                24 => {
                    let prs_expr_2 = self.stack.pop().unwrap().get_prs_expr();
                    let prs_expr_1 = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr4 { prs_expr: [prs_expr_1, prs_expr_2] }
                }
                25 => {
                    let prs_expr_2 = self.stack.pop().unwrap().get_prs_expr();
                    let prs_expr_1 = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr5 { prs_expr: [prs_expr_1, prs_expr_2] }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_prs_expr1")
            };
            let val = self.listener.exit_prs_expr(ctx);
            self.stack.push(SynValue::PrsExpr(val));
        }

        fn exit_prs_expr6(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                38 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr6 { prs_expr }
                }
                39 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxPrsExpr::PrsExpr7 { item }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_prs_expr6")
            };
            let val = self.listener.exit_prs_expr(ctx);
            self.stack.push(SynValue::PrsExpr(val));
        }

        fn exit_item(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                12 => {
                    let nonterminal = self.stack_t.pop().unwrap();
                    CtxItem::Item1 { nonterminal }
                }
                13 => {
                    let terminal = self.stack_t.pop().unwrap();
                    CtxItem::Item2 { terminal }
                }
                14 => {
                    let terminalcst = self.stack_t.pop().unwrap();
                    CtxItem::Item3 { terminalcst }
                }
                15 => {
                    let empty = self.stack_t.pop().unwrap();
                    CtxItem::Item4 { empty }
                }
                16 => {
                    let ltag = self.stack_t.pop().unwrap();
                    CtxItem::Item5 { ltag }
                }
                17 => {
                    CtxItem::Item6
                }
                18 => {
                    CtxItem::Item7
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_item")
            };
            let val = self.listener.exit_item(ctx);
            self.stack.push(SynValue::Item(val));
        }
    }

    // [rtsgen_parser]
}

// -------------------------------------------------------------------------
