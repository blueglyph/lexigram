// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

#![cfg(feature = "rtsgen")]

mod tests;

use std::collections::HashMap;
use std::io::Cursor;
use std::str::FromStr;
use iter_index::IndexerIterator;
use vectree::VecTree;
use crate::{CollectJoin, General, NameFixer, NameTransformer, SymbolTable};
use crate::dfa::TokenId;
use crate::grammar::{GrNode, GrTree, RuleTreeSet, Symbol, VarId};
use crate::io::CharReader;
use crate::lexer::{Lexer, TokenSpliterator};
use crate::log::{BufLog, LogStatus, Logger};
use crate::parser::Parser;
use crate::rtsgen::listener_types::*;
use crate::rtsgen::rtsgen_lexer::build_lexer;
use crate::rtsgen::rtsgen_parser::*;

static T_NAME_DICTIONARY: &[(&str, &str)] = &[
    ("+", "Add"), ("-", "Sub"), ("*", "Mul"), ("/", "Div"), ("%", "Percent"), ("++", "Inc"), ("--", "Dec"),
    ("<<", "Shl"), (">>", "Shr"), ("!", "Not"), ("^", "Exp"), ("~", "Tilde"),
    ("&", "And"), ("|", "Or"), ("&&", "And2"), ("||", "Or2"),
    ("=", "Eq"), ("==", "Eq2"), ("<", "Lt"), (">", "Gt"), ("<=", "LtEq"), (">=", "GtEq"), ("!=", "Neq"),
    (":=", "ColonEq"), ("+=", "AddEq"), ("-=", "SubEq"), ("*=", "MulEq"), ("/=", "DivEq"),
    ("(", "LPar"), (")", "RPar"), ("[", "LSBracket"), ("]", "RSBracker"), ("{", "LBracket"), ("}", "RBracket"),
    ("\"", "DQuote"), ("'", "Quote"), ("$", "Dollar"), ("?", "Question"), ("\\", "Backslash"),
    (":", "Colon"), (";", "SemiColon"), (".", "Dot"), (",", "Comma"), ("#", "Sharp"), ("@", "At"), ("´", "Tick"), ("`", "BTick"),
    ("\n", "EOL"), ("\r\n", "WinEOL"), ("\r", "CR"), ("\t", "Tab"), (" ", "Space"), ("\\", "Backslash"), ("\0", "Null"),
    ("->", "Arrow"), ("=>", "DArrow"),
];

pub struct RtsGen<'l, 'p> {
    lexer: Lexer<'l, Cursor<String>>,
    parser: Parser<'p>,
    t_name_dictionary: Option<HashMap<String, String>>,
}

impl RtsGen<'_, '_> {
    pub fn new() -> Self {
        let t_name_dictionary = Some(HashMap::from_iter(T_NAME_DICTIONARY.into_iter().map(|(a, b)| (a.to_string(), b.to_string()))));
        RtsGen::with_guess_names(t_name_dictionary)
    }

    pub fn with_guess_names(t_name_dictionary: Option<HashMap<String, String>>) -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        RtsGen { lexer, parser, t_name_dictionary }
    }

    pub fn parse(&mut self, text: String) -> Result<RuleTreeSet<General>, BufLog> {
        const VERBOSE_WRAPPER: bool = false;

        let mut wrapper = Wrapper::new(RGListener::new(self.t_name_dictionary.as_ref()), VERBOSE_WRAPPER);
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
    reserved_nt: Vec<VarId>,
    symbol_table: Option<SymbolTable>,
    t: HashMap<String, TokenId>,
    /// terminal information; `(String, Option<String>)` = (token name, optional string if not variable)
    tokens: Vec<(String, Option<String>)>,
    curr: Option<GrTree>,
    curr_name: Option<String>,
    curr_nt: Option<VarId>,
    t_name_dictionary: Option<&'a HashMap<String, String>>,
}

#[derive(Clone, Copy, PartialEq)]
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
    fn new(t_name_dictionary: Option<&'a HashMap<String, String>>) -> Self {
        RGListener {
            log: BufLog::new(),
            nt: HashMap::new(),
            nt_def_order: Vec::new(),
            rules: Vec::new(),
            reserved_nt: Vec::new(),
            symbol_table: None,
            t: HashMap::new(),
            tokens: Vec::new(),
            curr: None,
            curr_name: None,
            curr_nt: None,
            t_name_dictionary,
        }
    }

    pub fn make_rts(self) -> Result<RuleTreeSet<General>, BufLog> {
        let RGListener { log, rules, symbol_table, .. } = self;
        if log.has_no_errors() {
            let mut rts = RuleTreeSet::<General>::with_log(log);
            rts.set_symbol_table(symbol_table.unwrap());
            rts.set_start(0);
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

    fn invent_t_name(&self, tok: TokenId, value: &str) -> String {
        self.t_name_dictionary
            .and_then(|g| g.get(value).cloned())
            .unwrap_or_else(|| {
                if value.chars().any(|c| c.is_ascii_alphanumeric()) &&
                    value.chars().all(|c| c.is_ascii_alphanumeric() || c.is_whitespace() || c == '_')
                {
                    let name = value.chars().map(|c| if c.is_whitespace() { '_' } else { c }).collect::<String>();
                    if !value.as_bytes()[0].is_ascii_alphabetic() {
                        format!("Tok{}", name.to_camelcase())
                    } else {
                        name.to_camelcase()
                    }
                } else {
                    format!("Token{tok}")
                }
            })
    }

    /// Finalizes the rules and creates the symbol table.
    fn finalize_ruleset(&mut self) {
        let mut nt_name = vec![String::new(); self.nt.len()];
        let mut dest = vec![0; self.nt.len()];
        self.nt_def_order.iter().enumerate().for_each(|(i, &nt)| dest[nt as usize] = i);
        for (name, var) in &self.nt {
            nt_name[dest[*var as usize]] = name.clone();
        }

        // detects undefined nonterminals
        let mut undefined = self.nt.iter()
            .filter_map(|(name, &var)| if self.rules[var as usize].is_empty() { Some(name.to_string()) } else { None })
            .to_vec();
        if !undefined.is_empty() {
            undefined.sort(); // comes from hashmap
            self.log.add_error(format!("undefined nonterminals: {}", undefined.into_iter().map(|s| format!("'{s}'")).join(", ")));
            return;
        }

        // builds symbol table
        let mut symtab = SymbolTable::new();
        symtab.extend_nonterminals(nt_name);
        let mut t_name = vec![(String::new(), None); self.tokens.len()];
        let mut namefixer = NameFixer::new_empty();
        for (tok, (name, value_maybe)) in self.tokens.iter().enumerate().filter(|(_, (name, _))| !name.is_empty()) {
            namefixer.add(name.clone());
            t_name[tok] = (name.clone(), value_maybe.clone());
        }

        // puts names to constant terminals
        for (tok, (_, cst_maybe)) in self.tokens.iter().enumerate().filter(|(_, (name, _))| name.is_empty()) {
            let new_name = namefixer.get_unique_name(self.invent_t_name(tok as TokenId, cst_maybe.as_ref().unwrap()));
            t_name[tok] = (new_name, cst_maybe.clone());
        }
        symtab.extend_terminals(t_name);
        self.symbol_table = Some(symtab);

        // remove reserved nonterminals
        for v in &self.reserved_nt {
            self.rules[*v as usize].clear();
        }

        // reorders nonterminal IDs by order of definition rather than order of appearance
        self.rules = self.nt_def_order.iter()
            .map(|nt| std::mem::take(self.rules.get_mut(*nt as usize).unwrap()))
            .collect();
        for rule in &mut self.rules {
            for mut node in rule.iter_depth_simple_mut() {
                match *node {
                    GrNode::Symbol(Symbol::NT(ref mut old))
                    | GrNode::LForm(ref mut old) => *old = *dest.get(*old as usize).unwrap_or(&(*old as usize)) as VarId,
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

    fn exit(&mut self, _ruleset: SynFile) {
        self.finalize_ruleset();
    }

    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        SynFile()
    }

    fn exit_decls(&mut self, _ctx: CtxDecls) -> SynDecls {
        SynDecls()
    }

    fn exit_decl(&mut self, _ctx: CtxDecl) -> SynDecl {
        SynDecl()
    }

    fn exit_decl_terminal(&mut self, ctx: CtxDeclTerminal) -> SynDeclTerminal {
        let (terminal, value_maybe) = match ctx {
            // decl_terminal -> Terminal "=" TerminalCst
            CtxDeclTerminal::DeclTerminal1 { terminal, terminalcst } => {
                (terminal, Some(terminalcst[1..terminalcst.len() - 1].to_string()))
            }
            // decl_terminal -> Terminal
            CtxDeclTerminal::DeclTerminal2 { terminal } => {
                (terminal, None)
            }
        };
        let (is_new, _tok) = self.get_or_create_t(terminal.clone());
        if is_new == IsNew::Yes {
            self.tokens.push((terminal, value_maybe));
        } else {
            self.log.add_error(format!("in token declarations: token '{terminal}' has already been declared"));
        }
        SynDeclTerminal()
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
        let mut error = false;
        assert_eq!(self.curr_nt, None);
        let var = if let Some(&var) = self.nt.get(&nonterminal) {
            if !self.rules[var as usize].is_empty() {
                error = true;
                self.log.add_error(format!("nonterminal '{nonterminal}' is defined multiple times"));
            }
            var
        } else {
            let var = self.get_or_create_nt(nonterminal.clone());
            var
        };
        if !error { self.nt_def_order.push(var); }
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
            // item -> NTx (NT symbol without any check or creation process)
            CtxItem::Item2 { ntx } => {
                let x = match VarId::from_str(&ntx[3..ntx.len() - 1]) {
                    Ok(x) => x,
                    Err(e) => {
                        self.log.add_error(format!("in {}, integer literal {ntx} can't be parsed: {e}", self.curr_name.as_ref().unwrap()));
                        VarId::MAX
                    }
                };
                self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::NT(x)))
            }
            // item -> Terminal
            CtxItem::Item3 { terminal } => {
                let (is_new, tok) = self.get_or_create_t(terminal.clone());
                if let IsNew::Yes = is_new {
                    self.tokens.push((terminal, None));
                }
                self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::T(tok)))
            }
            // item -> TerminalCst
            CtxItem::Item4 { terminalcst } => {
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
            // item -> Tx (T symbol without any check or creation process)
            CtxItem::Item5 { tx } => {
                let x = match VarId::from_str(&tx[2..tx.len() - 1]) {
                    Ok(x) => x,
                    Err(e) => {
                        self.log.add_error(format!("in {}, integer literal {tx} can't be parsed: {e}", self.curr_name.as_ref().unwrap()));
                        VarId::MAX
                    }
                };
                self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::T(x)))
            }
            // item -> Empty
            CtxItem::Item6 { .. } =>
                self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::Empty)),
            // item -> LTag
            CtxItem::Item7 { ltag } => {
                // `ltag` contains "<L=name>" or "<L>"
                let var = if ltag.len() > 3 {
                    let name = &ltag[3..ltag.len()-1];
                    let var = self.get_or_create_nt(name.to_string());
                    self.nt_def_order.push(var);
                    self.rules[var as usize].add_root(GrNode::Symbol(Symbol::Empty));
                    self.reserved_nt.push(var);
                    var
                } else {
                    self.curr_nt.unwrap()
                };
                self.curr.as_mut().unwrap().add(None, GrNode::LForm(var))
            }
            // item -> "<P>"
            CtxItem::Item8 => self.curr.as_mut().unwrap().add(None, GrNode::PrecEq),
            // item -> "<R>"
            CtxItem::Item9 => self.curr.as_mut().unwrap().add(None, GrNode::RAssoc),
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
    use crate::grammar::VarId;

    /// User-defined type for `file`
    #[derive(Debug, PartialEq)] pub struct SynFile();
    /// User-defined type for `decls`
    #[derive(Debug, PartialEq)] pub struct SynDecls();
    /// User-defined type for `decl`
    #[derive(Debug, PartialEq)] pub struct SynDecl();
    /// User-defined type for `decl_terminal`
    #[derive(Debug, PartialEq)] pub struct SynDeclTerminal();
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
    use crate as lexigram_lib;

    // [rtsgen_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_lib::dfa::{StateId, Terminal, ActionOption, ModeOption};
    use lexigram_lib::lexer::Lexer;
    use lexigram_lib::lexergen::GroupId;
    use lexigram_lib::segments::{Seg, SegMap};

    const NBR_GROUPS: u32 = 42;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 22;
    const NBR_STATES: StateId = 54;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         30,  30,  30,  30,  30,  30,  30,  30,  30,  22,  38,  30,  30,  38,  30,  30,   // 0-15
         30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,   // 16-31
          0,  30,   1,  30,  30,  30,   2,  30,   3,   4,   5,   6,   7,   8,  30,   9,   // 32-47
         36,  29,  29,  29,  29,  29,  29,  29,  29,  29,  30,  10,  11,  12,  23,  13,   // 48-63
         30,  28,  28,  28,  28,  28,  28,  33,  33,  33,  33,  33,  14,  33,  15,  33,   // 64-79
         26,  33,  27,  33,  16,  33,  33,  33,  33,  33,  33,  30,  24,  30,  30,  34,   // 80-95
         30,  32,  32,  32,  32,  40,  32,  35,  35,  35,  35,  39,  35,  35,  41,  37,   // 96-111
         35,  35,  17,  35,  18,  31,  35,  35,  35,  35,  35,  19,  20,  25,  30,  30,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 2] = [
        ('ε', 21),
        ('€', 21),
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 4] = [
        (Seg(128, 948), 30),
        (Seg(950, 8363), 30),
        (Seg(8365, 55295), 30),
        (Seg(57344, 1114111), 30),
    ];
    static TERMINAL_TABLE: [Terminal;32] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 2269] = [
         22,   1,  23,  24,  25,  26,  27,  28,   2,   3,  29,   4,  30,  31,  32,  33,  34,  35,  36,   5,  37,  38,  22,  54,  54,  54,  32,  32,  32,  54,  54,  35,  35,  32,  54,  35,  54,  35,  22,  35,  35,  35, // state 0
         13,  54,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  54,  13,  14,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  54,  13,  13,  13, // state 1
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  41,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 2
         54,  54,  54,  54,  54,   6,  54,  54,  54,  39,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 3
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,   8,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,   9,  10,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 4
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  38,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 5
          6,   6,   6,   6,   6,   7,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6, // state 6
          6,   6,   6,   6,   6,   7,   6,   6,   6,  40,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6, // state 7
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  11,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  43,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 8
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  44,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 9
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  45,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 10
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  12,  12,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  12,  12,  54,  54,  12,  54,  12,  54,  12,  12,  12, // state 11
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  12,  12,  54,  54,  54,  54,  43,  54,  54,  54,  54,  54,  12,  54,  12,  12,  54,  12,  12,  12,  12,  54,  12,  12,  12, // state 12
         13,  50,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  54,  13,  14,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  54,  13,  13,  13, // state 13
         54,  13,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  13,  13,  54,  54,  54,  54,  54,  13,  54,  54,  54,  54,  54,  54,  15,  54,  54,  54,  54,  54,  54,  54,  54,  54,  13, // state 14
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  16,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 15
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  17,  17,  54,  54,  17,  54,  54,  54,  17,  54,  54,  54,  17,  54, // state 16
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  13,  54,  54,  17,  17,  54,  54,  17,  54,  54,  54,  17,  54,  54,  54,  17,  54, // state 17
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  19,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 18
         54,  54,  54,  54,  52,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  19,  54,  54,  54,  54,  54,  54,  19,  54,  54,  54,  54,  54, // state 19
         54,  54,  54,  54,  53,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  20,  54,  54,  54,  54,  54,  54,  20,  54,  54,  54,  54,  54, // state 20
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  20,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 21
         22,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  22,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  22,  54,  54,  54, // state 22 <skip>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 23 <end:2>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 24 <end:10>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 25 <end:11>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 26 <end:5>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 27 <end:4>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 28 <end:9>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 29 <end:12>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  42,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 30 <end:8>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 31 <end:6>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  32,  32,  32,  32,  32,  54,  54,  54,  54,  54,  54,  54,  32,  32,  32,  32,  54,  32,  32,  32,  32,  32,  32,  32,  54,  32,  32,  32, // state 32 <end:18>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  32,  32,  51,  32,  32,  54,  54,  54,  54,  54,  54,  54,  32,  32,  32,  32,  54,  32,  32,  32,  32,  32,  32,  32,  54,  32,  32,  32, // state 33 <end:18>
         54,  54,  54,  18,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  32,  32,  32,  32,  32,  54,  54,  54,  54,  54,  54,  54,  32,  32,  32,  32,  54,  32,  32,  32,  32,  32,  32,  32,  54,  32,  32,  32, // state 34 <end:18>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  35,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  54,  35,  35,  54,  35,  35,  35,  35,  54,  35,  35,  35, // state 35 <end:19>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  35,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  54,  35,  35,  54,  35,  35,  35,  46,  54,  35,  35,  35, // state 36 <end:19>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 37 <end:3>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 38 <end:7>
         39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,  54,  39,  39,  39, // state 39 <skip>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 40 <skip>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 41 <end:0>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 42 <end:1>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 43 <end:13>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 44 <end:14>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 45 <end:15>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  35,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  54,  35,  35,  54,  35,  35,  35,  35,  54,  47,  35,  35, // state 46 <end:19>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  35,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  54,  35,  35,  54,  35,  35,  35,  35,  54,  35,  48,  35, // state 47 <end:19>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  35,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  54,  35,  35,  54,  35,  35,  35,  35,  54,  35,  35,  49, // state 48 <end:19>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  35,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  35,  54,  35,  35,  54,  35,  35,  35,  35,  54,  35,  35,  35, // state 49 <end:16>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 50 <end:17>
         54,  54,  54,  21,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  32,  32,  32,  32,  32,  54,  54,  54,  54,  54,  54,  54,  32,  32,  32,  32,  54,  32,  32,  32,  32,  32,  32,  32,  54,  32,  32,  32, // state 51 <end:18>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 52 <end:20>
         54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,  54, // state 53 <end:21>
         54 // error group in [nbr_state * nbr_group + nbr_group]
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
    use crate as lexigram_lib;

    // [rtsgen_parser]

    use lexigram_lib::{CollectJoin, FixedSymTable, grammar::{AltId, Alternative, Symbol, VarId}, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 22;
    const PARSER_NUM_NT: usize = 23;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Arrow", Some("->")), ("DArrow", Some("=>")), ("Concat", Some("&")), ("Or", Some("|")), ("Plus", Some("+")), ("Star", Some("*")), ("Question", Some("?")), ("Empty", None), ("Equal", Some("=")), ("Comma", Some(",")), ("LPar", Some("(")), ("RPar", Some(")")), ("Semicolon", Some(";")), ("LTag", None), ("PTag", Some("<P>")), ("RTag", Some("<R>")), ("Token", Some("token")), ("TerminalCst", None), ("Terminal", None), ("Nonterminal", None), ("Tx", None), ("NTx", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "decls", "decl_iter", "decl", "decl_terminal", "ruleset", "rule_iter", "rule", "rule_nt", "rts_expr", "rts_children", "prs_expr", "item", "decl_1", "rts_children_1", "prs_expr_1", "prs_expr_2", "prs_expr_3", "prs_expr_4", "prs_expr_5", "prs_expr_6", "decl_terminal_1", "rule_1"];
    static ALT_VAR: [VarId; 55] = [0, 1, 2, 2, 3, 4, 5, 6, 6, 7, 8, 9, 9, 9, 9, 9, 9, 10, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 14, 14, 15, 15, 15, 15, 15, 15, 16, 17, 17, 17, 17, 17, 18, 19, 19, 19, 19, 20, 20, 21, 21, 22, 22];
    static ALTERNATIVES: [&[Symbol]; 55] = [&[Symbol::NT(1), Symbol::NT(5)], &[Symbol::NT(2)], &[Symbol::NT(3), Symbol::NT(2)], &[Symbol::Empty], &[Symbol::T(16), Symbol::NT(4), Symbol::NT(13), Symbol::T(12)], &[Symbol::T(18), Symbol::NT(21)], &[Symbol::NT(6)], &[Symbol::NT(7), Symbol::NT(6)], &[Symbol::Empty], &[Symbol::NT(8), Symbol::NT(22)], &[Symbol::T(19)], &[Symbol::T(2), Symbol::NT(10)], &[Symbol::T(3), Symbol::NT(10)], &[Symbol::T(4), Symbol::NT(10)], &[Symbol::T(5), Symbol::NT(10)], &[Symbol::T(6), Symbol::NT(10)], &[Symbol::NT(12)], &[Symbol::T(10), Symbol::NT(14), Symbol::T(11)], &[Symbol::NT(20), Symbol::NT(15)], &[Symbol::T(19)], &[Symbol::T(21)], &[Symbol::T(18)], &[Symbol::T(17)], &[Symbol::T(20)], &[Symbol::T(7)], &[Symbol::T(13)], &[Symbol::T(14)], &[Symbol::T(15)], &[Symbol::T(9), Symbol::NT(4), Symbol::NT(13)], &[Symbol::Empty], &[Symbol::NT(9), Symbol::NT(14)], &[Symbol::Empty], &[Symbol::T(4), Symbol::NT(15)], &[Symbol::T(5), Symbol::NT(15)], &[Symbol::T(6), Symbol::NT(15)], &[Symbol::NT(18), Symbol::NT(15)], &[Symbol::T(3), Symbol::NT(16), Symbol::NT(15)], &[Symbol::Empty], &[Symbol::NT(20), Symbol::NT(17)], &[Symbol::T(4), Symbol::NT(17)], &[Symbol::T(5), Symbol::NT(17)], &[Symbol::T(6), Symbol::NT(17)], &[Symbol::NT(18), Symbol::NT(17)], &[Symbol::Empty], &[Symbol::NT(20), Symbol::NT(19)], &[Symbol::T(4), Symbol::NT(19)], &[Symbol::T(5), Symbol::NT(19)], &[Symbol::T(6), Symbol::NT(19)], &[Symbol::Empty], &[Symbol::T(10), Symbol::NT(11), Symbol::T(11)], &[Symbol::NT(12)], &[Symbol::T(8), Symbol::T(17)], &[Symbol::Empty], &[Symbol::T(0), Symbol::NT(11), Symbol::T(12)], &[Symbol::T(1), Symbol::NT(9), Symbol::T(12)]];
    static PARSING_TABLE: [AltId; 529] = [55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 0, 55, 55, 0, 55, 55, 0, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 1, 55, 55, 1, 55, 55, 1, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 2, 55, 55, 3, 55, 55, 3, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 4, 55, 55, 56, 55, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 55, 55, 56, 55, 55, 55, 55, 55, 5, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 6, 55, 55, 6, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 7, 55, 55, 8, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 9, 55, 55, 56, 56, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 10, 55, 55, 55, 55, 55, 11, 12, 13, 14, 15, 16, 55, 55, 55, 56, 56, 16, 16, 16, 55, 16, 16, 16, 16, 16, 55, 55, 55, 56, 56, 56, 56, 56, 56, 55, 55, 17, 56, 56, 56, 56, 56, 55, 56, 56, 56, 56, 56, 55, 55, 55, 55, 55, 55, 55, 55, 18, 55, 55, 18, 56, 56, 18, 18, 18, 55, 18, 18, 18, 18, 18, 55, 55, 55, 56, 56, 56, 56, 56, 24, 55, 55, 56, 56, 56, 25, 26, 27, 55, 22, 21, 19, 23, 20, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 28, 55, 55, 29, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 30, 30, 30, 30, 30, 30, 55, 55, 55, 31, 55, 30, 30, 30, 55, 30, 30, 30, 30, 30, 55, 55, 55, 55, 36, 32, 33, 34, 35, 55, 55, 35, 37, 37, 35, 35, 35, 55, 35, 35, 35, 35, 35, 55, 55, 55, 55, 56, 56, 56, 56, 38, 55, 55, 38, 56, 56, 38, 38, 38, 55, 38, 38, 38, 38, 38, 55, 55, 55, 55, 43, 39, 40, 41, 42, 55, 55, 42, 43, 43, 42, 42, 42, 55, 42, 42, 42, 42, 42, 55, 55, 55, 55, 56, 56, 56, 56, 44, 55, 55, 44, 56, 56, 44, 44, 44, 55, 44, 44, 44, 44, 44, 55, 55, 55, 55, 48, 45, 46, 47, 48, 55, 55, 48, 48, 48, 48, 48, 48, 55, 48, 48, 48, 48, 48, 55, 55, 55, 55, 56, 56, 56, 56, 50, 55, 55, 49, 56, 56, 50, 50, 50, 55, 50, 50, 50, 50, 50, 55, 55, 55, 55, 55, 55, 55, 55, 55, 51, 52, 55, 55, 52, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 53, 54, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 55, 55, 56];
    static OPCODES: [&[OpCode]; 55] = [&[OpCode::Exit(0), OpCode::NT(5), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Loop(2), OpCode::Exit(2), OpCode::NT(3)], &[OpCode::Exit(3)], &[OpCode::Exit(4), OpCode::T(12), OpCode::NT(13), OpCode::NT(4), OpCode::T(16)], &[OpCode::NT(21), OpCode::T(18)], &[OpCode::Exit(6), OpCode::NT(6)], &[OpCode::Loop(6), OpCode::Exit(7), OpCode::NT(7)], &[OpCode::Exit(8)], &[OpCode::NT(22), OpCode::NT(8)], &[OpCode::Exit(10), OpCode::T(19)], &[OpCode::Exit(11), OpCode::NT(10), OpCode::T(2)], &[OpCode::Exit(12), OpCode::NT(10), OpCode::T(3)], &[OpCode::Exit(13), OpCode::NT(10), OpCode::T(4)], &[OpCode::Exit(14), OpCode::NT(10), OpCode::T(5)], &[OpCode::Exit(15), OpCode::NT(10), OpCode::T(6)], &[OpCode::Exit(16), OpCode::NT(12)], &[OpCode::Exit(17), OpCode::T(11), OpCode::NT(14), OpCode::T(10)], &[OpCode::NT(15), OpCode::Exit(18), OpCode::NT(20)], &[OpCode::Exit(19), OpCode::T(19)], &[OpCode::Exit(20), OpCode::T(21)], &[OpCode::Exit(21), OpCode::T(18)], &[OpCode::Exit(22), OpCode::T(17)], &[OpCode::Exit(23), OpCode::T(20)], &[OpCode::Exit(24), OpCode::T(7)], &[OpCode::Exit(25), OpCode::T(13)], &[OpCode::Exit(26), OpCode::T(14)], &[OpCode::Exit(27), OpCode::T(15)], &[OpCode::Loop(13), OpCode::Exit(28), OpCode::NT(4), OpCode::T(9)], &[OpCode::Exit(29)], &[OpCode::Loop(14), OpCode::Exit(30), OpCode::NT(9)], &[OpCode::Exit(31)], &[OpCode::Loop(15), OpCode::Exit(32), OpCode::T(4)], &[OpCode::Loop(15), OpCode::Exit(33), OpCode::T(5)], &[OpCode::Loop(15), OpCode::Exit(34), OpCode::T(6)], &[OpCode::Loop(15), OpCode::Exit(35), OpCode::NT(18)], &[OpCode::Loop(15), OpCode::Exit(36), OpCode::NT(16), OpCode::T(3)], &[OpCode::Exit(37)], &[OpCode::NT(17), OpCode::Exit(38), OpCode::NT(20)], &[OpCode::Loop(17), OpCode::Exit(39), OpCode::T(4)], &[OpCode::Loop(17), OpCode::Exit(40), OpCode::T(5)], &[OpCode::Loop(17), OpCode::Exit(41), OpCode::T(6)], &[OpCode::Loop(17), OpCode::Exit(42), OpCode::NT(18)], &[OpCode::Exit(43)], &[OpCode::NT(19), OpCode::Exit(44), OpCode::NT(20)], &[OpCode::Loop(19), OpCode::Exit(45), OpCode::T(4)], &[OpCode::Loop(19), OpCode::Exit(46), OpCode::T(5)], &[OpCode::Loop(19), OpCode::Exit(47), OpCode::T(6)], &[OpCode::Exit(48)], &[OpCode::Exit(49), OpCode::T(11), OpCode::NT(11), OpCode::T(10)], &[OpCode::Exit(50), OpCode::NT(12)], &[OpCode::Exit(51), OpCode::T(17), OpCode::T(8)], &[OpCode::Exit(52)], &[OpCode::Exit(53), OpCode::T(12), OpCode::NT(11), OpCode::T(0)], &[OpCode::Exit(54), OpCode::T(12), OpCode::NT(9), OpCode::T(1)]];
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
    pub enum CtxFile {
        /// `file -> decls ruleset`
        File { decls: SynDecls, ruleset: SynRuleset },
    }
    #[derive(Debug)]
    pub enum CtxDecls {
        /// `decls -> (<L> decl)*`
        Decls,
    }
    #[derive(Debug)]
    pub enum CtxDeclIter {
        /// `<L> decl` iteration in `decls -> ( ►► <L> decl ◄◄ )*`
        DeclIter { decl: SynDecl },
    }
    #[derive(Debug)]
    pub enum CtxDecl {
        /// `decl -> "token" decl_terminal ("," decl_terminal)* ";"`
        Decl { decl_terminal: SynDeclTerminal, star: SynDecl1 },
    }
    #[derive(Debug)]
    pub enum CtxDeclTerminal {
        /// `decl_terminal -> Terminal "=" TerminalCst`
        DeclTerminal1 { terminal: String, terminalcst: String },
        /// `decl_terminal -> Terminal`
        DeclTerminal2 { terminal: String },
    }
    #[derive(Debug)]
    pub enum CtxRuleset {
        /// `ruleset -> (<L> rule)*`
        Ruleset,
    }
    #[derive(Debug)]
    pub enum CtxRuleIter {
        /// `<L> rule` iteration in `ruleset -> ( ►► <L> rule ◄◄ )*`
        RuleIter { rule: SynRule },
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
        /// `item -> NTx`
        Item2 { ntx: String },
        /// `item -> Terminal`
        Item3 { terminal: String },
        /// `item -> TerminalCst`
        Item4 { terminalcst: String },
        /// `item -> Tx`
        Item5 { tx: String },
        /// `item -> Empty`
        Item6 { empty: String },
        /// `item -> LTag`
        Item7 { ltag: String },
        /// `item -> "<P>"`
        Item8,
        /// `item -> "<R>"`
        Item9,
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `file`
    // #[derive(Debug, PartialEq)] pub struct SynFile();
    // /// User-defined type for `decls`
    // #[derive(Debug, PartialEq)] pub struct SynDecls();
    // /// User-defined type for `decl`
    // #[derive(Debug, PartialEq)] pub struct SynDecl();
    // /// User-defined type for `decl_terminal`
    // #[derive(Debug, PartialEq)] pub struct SynDeclTerminal();
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
    /// Computed `("," decl_terminal)*` array in `decl -> "token" decl_terminal  ►► ("," decl_terminal)* ◄◄  ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynDecl1(pub Vec<SynDeclTerminal>);
    /// Computed `rts_expr*` array in `rts_children -> "("  ►► rts_expr* ◄◄  ")"`
    #[derive(Debug, PartialEq)]
    pub struct SynRtsChildren1(pub Vec<SynRtsExpr>);

    #[derive(Debug)]
    enum SynValue { File(SynFile), Decls(SynDecls), Decl(SynDecl), DeclTerminal(SynDeclTerminal), Ruleset(SynRuleset), Rule(SynRule), RuleNt(SynRuleNt), RtsExpr(SynRtsExpr), RtsChildren(SynRtsChildren), PrsExpr(SynPrsExpr), Item(SynItem), Decl1(SynDecl1), RtsChildren1(SynRtsChildren1) }

    impl SynValue {
        fn get_file(self) -> SynFile {
            if let SynValue::File(val) = self { val } else { panic!() }
        }
        fn get_decls(self) -> SynDecls {
            if let SynValue::Decls(val) = self { val } else { panic!() }
        }
        fn get_decl(self) -> SynDecl {
            if let SynValue::Decl(val) = self { val } else { panic!() }
        }
        fn get_decl_terminal(self) -> SynDeclTerminal {
            if let SynValue::DeclTerminal(val) = self { val } else { panic!() }
        }
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
        fn get_decl1(self) -> SynDecl1 {
            if let SynValue::Decl1(val) = self { val } else { panic!() }
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
        fn exit(&mut self, _file: SynFile) {}
        fn init_file(&mut self) {}
        fn exit_file(&mut self, _ctx: CtxFile) -> SynFile;
        fn init_decls(&mut self) {}
        fn exit_decls(&mut self, _ctx: CtxDecls) -> SynDecls;
        fn init_decl_iter(&mut self) {}
        fn exit_decl_iter(&mut self, _ctx: CtxDeclIter) {}
        fn init_decl(&mut self) {}
        fn exit_decl(&mut self, _ctx: CtxDecl) -> SynDecl;
        fn init_decl_terminal(&mut self) {}
        fn exit_decl_terminal(&mut self, _ctx: CtxDeclTerminal) -> SynDeclTerminal;
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
                        0 => self.listener.init_file(),             // file
                        1 => self.listener.init_decls(),            // decls
                        2 => self.listener.init_decl_iter(),        // decl_iter
                        3 => self.listener.init_decl(),             // decl
                        13 => self.init_decl1(),                    // decl_1
                        4 => self.listener.init_decl_terminal(),    // decl_terminal
                        21 => {}                                    // decl_terminal_1
                        5 => self.listener.init_ruleset(),          // ruleset
                        6 => self.listener.init_rule_iter(),        // rule_iter
                        7 => self.listener.init_rule(),             // rule
                        22 => {}                                    // rule_1
                        8 => self.listener.init_rule_nt(),          // rule_nt
                        9 => self.listener.init_rts_expr(),         // rts_expr
                        10 => self.listener.init_rts_children(),    // rts_children
                        14 => self.init_rts_children1(),            // rts_children_1
                        11 => self.listener.init_prs_expr(),        // prs_expr
                        15 ..= 20 => {}                             // prs_expr_1, prs_expr_2, prs_expr_3, prs_expr_4, prs_expr_5, prs_expr_6
                        12 => self.listener.init_item(),            // item
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_file(),                      // file -> decls ruleset
                        1 => self.exit_decls(),                     // decls -> decl_iter
                        2 => self.exit_decl_iter(),                 // decl_iter -> <L> decl decl_iter
                        3 => {}                                     // decl_iter -> <L> ε (not used)
                        4 => self.exit_decl(),                      // decl -> "token" decl_terminal decl_1 ";"
                        28 => self.exit_decl1(),                    // decl_1 -> "," decl_terminal decl_1
                        29 => {}                                    // decl_1 -> ε
                        51 |                                        // decl_terminal_1 -> "=" TerminalCst
                        52 => self.exit_decl_terminal(alt_id),      // decl_terminal_1 -> ε
                     /* 5 */                                        // decl_terminal -> Terminal decl_terminal_1 (never called)
                        6 => self.exit_ruleset(),                   // ruleset -> rule_iter
                        7 => self.exit_rule_iter(),                 // rule_iter -> <L> rule rule_iter
                        8 => {}                                     // rule_iter -> <L> ε (not used)
                        53 |                                        // rule_1 -> "->" prs_expr ";"
                        54 => self.exit_rule(alt_id),               // rule_1 -> "=>" rts_expr ";"
                     /* 9 */                                        // rule -> rule_nt rule_1 (never called)
                        10 => self.exit_rule_nt(),                  // rule_nt -> Nonterminal
                        11 |                                        // rts_expr -> "&" rts_children
                        12 |                                        // rts_expr -> "|" rts_children
                        13 |                                        // rts_expr -> "+" rts_children
                        14 |                                        // rts_expr -> "*" rts_children
                        15 |                                        // rts_expr -> "?" rts_children
                        16 => self.exit_rts_expr(alt_id),           // rts_expr -> item
                        17 => self.exit_rts_children(),             // rts_children -> "(" rts_children_1 ")"
                        30 => self.exit_rts_children1(),            // rts_children_1 -> rts_expr rts_children_1
                        31 => {}                                    // rts_children_1 -> ε
                        32 |                                        // prs_expr_1 -> "+" prs_expr_1
                        33 |                                        // prs_expr_1 -> "*" prs_expr_1
                        34 |                                        // prs_expr_1 -> "?" prs_expr_1
                        35 |                                        // prs_expr_1 -> prs_expr_4 prs_expr_1
                        36 => self.exit_prs_expr1(alt_id),          // prs_expr_1 -> "|" prs_expr_2 prs_expr_1
                        39 |                                        // prs_expr_3 -> "+" prs_expr_3 (duplicate of 32)
                        45 => self.exit_prs_expr1(32),              // prs_expr_5 -> "+" prs_expr_5 (duplicate of 32)
                        40 |                                        // prs_expr_3 -> "*" prs_expr_3 (duplicate of 33)
                        46 => self.exit_prs_expr1(33),              // prs_expr_5 -> "*" prs_expr_5 (duplicate of 33)
                        41 |                                        // prs_expr_3 -> "?" prs_expr_3 (duplicate of 34)
                        47 => self.exit_prs_expr1(34),              // prs_expr_5 -> "?" prs_expr_5 (duplicate of 34)
                        42 => self.exit_prs_expr1(35),              // prs_expr_3 -> prs_expr_4 prs_expr_3 (duplicate of 35)
                        49 |                                        // prs_expr_6 -> "(" prs_expr ")"
                        50 => self.exit_prs_expr6(alt_id),          // prs_expr_6 -> item
                        18 => {}                                    // prs_expr -> prs_expr_6 prs_expr_1 (not used)
                        37 => {}                                    // prs_expr_1 -> ε (not used)
                        38 => {}                                    // prs_expr_2 -> prs_expr_6 prs_expr_3 (not used)
                        43 => {}                                    // prs_expr_3 -> ε (not used)
                        44 => {}                                    // prs_expr_4 -> prs_expr_6 prs_expr_5 (not used)
                        48 => {}                                    // prs_expr_5 -> ε (not used)
                        19 |                                        // item -> Nonterminal
                        20 |                                        // item -> NTx
                        21 |                                        // item -> Terminal
                        22 |                                        // item -> TerminalCst
                        23 |                                        // item -> Tx
                        24 |                                        // item -> Empty
                        25 |                                        // item -> LTag
                        26 |                                        // item -> "<P>"
                        27 => self.exit_item(alt_id),               // item -> "<R>"
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
            let file = self.stack.pop().unwrap().get_file();
            self.listener.exit(file);
        }

        fn exit_file(&mut self) {
            let ruleset = self.stack.pop().unwrap().get_ruleset();
            let decls = self.stack.pop().unwrap().get_decls();
            let val = self.listener.exit_file(CtxFile::File { decls, ruleset });
            self.stack.push(SynValue::File(val));
        }

        fn exit_decls(&mut self) {
            let val = self.listener.exit_decls(CtxDecls::Decls);
            self.stack.push(SynValue::Decls(val));
        }

        fn exit_decl_iter(&mut self) {
            let decl = self.stack.pop().unwrap().get_decl();
            self.listener.exit_decl_iter(CtxDeclIter::DeclIter { decl });
        }

        fn exit_decl(&mut self) {
            let star = self.stack.pop().unwrap().get_decl1();
            let decl_terminal = self.stack.pop().unwrap().get_decl_terminal();
            let val = self.listener.exit_decl(CtxDecl::Decl { decl_terminal, star });
            self.stack.push(SynValue::Decl(val));
        }

        fn init_decl1(&mut self) {
            let val = SynDecl1(Vec::new());
            self.stack.push(SynValue::Decl1(val));
        }

        fn exit_decl1(&mut self) {
            let decl_terminal = self.stack.pop().unwrap().get_decl_terminal();
            let mut star_it = self.stack.pop().unwrap().get_decl1();
            star_it.0.push(decl_terminal);
            self.stack.push(SynValue::Decl1(star_it));
        }

        fn exit_decl_terminal(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                51 => {
                    let terminalcst = self.stack_t.pop().unwrap();
                    let terminal = self.stack_t.pop().unwrap();
                    CtxDeclTerminal::DeclTerminal1 { terminal, terminalcst }
                }
                52 => {
                    let terminal = self.stack_t.pop().unwrap();
                    CtxDeclTerminal::DeclTerminal2 { terminal }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_decl_terminal")
            };
            let val = self.listener.exit_decl_terminal(ctx);
            self.stack.push(SynValue::DeclTerminal(val));
        }

        fn exit_ruleset(&mut self) {
            let val = self.listener.exit_ruleset(CtxRuleset::Ruleset);
            self.stack.push(SynValue::Ruleset(val));
        }

        fn exit_rule_iter(&mut self) {
            let rule = self.stack.pop().unwrap().get_rule();
            self.listener.exit_rule_iter(CtxRuleIter::RuleIter { rule });
        }

        fn exit_rule(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                53 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    let rule_nt = self.stack.pop().unwrap().get_rule_nt();
                    CtxRule::Rule1 { rule_nt, prs_expr }
                }
                54 => {
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
                11 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr1 { rts_children }
                }
                12 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr2 { rts_children }
                }
                13 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr3 { rts_children }
                }
                14 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr4 { rts_children }
                }
                15 => {
                    let rts_children = self.stack.pop().unwrap().get_rts_children();
                    CtxRtsExpr::RtsExpr5 { rts_children }
                }
                16 => {
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
                32 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr1 { prs_expr }
                }
                33 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr2 { prs_expr }
                }
                34 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr3 { prs_expr }
                }
                35 => {
                    let prs_expr_2 = self.stack.pop().unwrap().get_prs_expr();
                    let prs_expr_1 = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr4 { prs_expr: [prs_expr_1, prs_expr_2] }
                }
                36 => {
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
                49 => {
                    let prs_expr = self.stack.pop().unwrap().get_prs_expr();
                    CtxPrsExpr::PrsExpr6 { prs_expr }
                }
                50 => {
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
                19 => {
                    let nonterminal = self.stack_t.pop().unwrap();
                    CtxItem::Item1 { nonterminal }
                }
                20 => {
                    let ntx = self.stack_t.pop().unwrap();
                    CtxItem::Item2 { ntx }
                }
                21 => {
                    let terminal = self.stack_t.pop().unwrap();
                    CtxItem::Item3 { terminal }
                }
                22 => {
                    let terminalcst = self.stack_t.pop().unwrap();
                    CtxItem::Item4 { terminalcst }
                }
                23 => {
                    let tx = self.stack_t.pop().unwrap();
                    CtxItem::Item5 { tx }
                }
                24 => {
                    let empty = self.stack_t.pop().unwrap();
                    CtxItem::Item6 { empty }
                }
                25 => {
                    let ltag = self.stack_t.pop().unwrap();
                    CtxItem::Item7 { ltag }
                }
                26 => {
                    CtxItem::Item8
                }
                27 => {
                    CtxItem::Item9
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
