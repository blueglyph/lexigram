// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
// [gramparser]

use gramparser_types::*;
use lexigram_lib::{CollectJoin, FixedSymTable, grammar::{AltId, Alternative, Symbol, VarId}, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};

const PARSER_NUM_T: usize = 14;
const PARSER_NUM_NT: usize = 14;
static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Colon", Some(":")), ("Lparen", Some("(")), ("Or", Some("|")), ("Plus", Some("+")), ("Question", Some("?")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Grammar", Some("grammar")), ("SymEof", Some("EOF")), ("Lform", None), ("Rform", Some("<R>")), ("Pform", Some("<P>")), ("Id", None)];
static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "header", "rules", "rule", "rule_name", "prod", "prod_term", "prod_factor", "prod_atom", "prod_term_1", "rules_1", "prod_1", "rule_1", "prod_factor_1"];
static ALT_VAR: [VarId; 25] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 13, 13];
static ALTERNATIVES: [&[Symbol]; 25] = [&[Symbol::NT(1), Symbol::NT(2)], &[Symbol::T(8), Symbol::T(13), Symbol::T(6)], &[Symbol::NT(3), Symbol::NT(10)], &[Symbol::NT(4), Symbol::T(0), Symbol::NT(5), Symbol::NT(12)], &[Symbol::T(13)], &[Symbol::NT(6), Symbol::NT(11)], &[Symbol::NT(9)], &[Symbol::NT(8), Symbol::NT(13)], &[Symbol::T(13)], &[Symbol::T(10)], &[Symbol::T(11)], &[Symbol::T(12)], &[Symbol::T(1), Symbol::NT(5), Symbol::T(5)], &[Symbol::NT(7), Symbol::NT(9)], &[Symbol::Empty], &[Symbol::NT(3), Symbol::NT(10)], &[Symbol::Empty], &[Symbol::T(2), Symbol::NT(6), Symbol::NT(11)], &[Symbol::Empty], &[Symbol::T(6)], &[Symbol::T(9), Symbol::T(6)], &[Symbol::T(3)], &[Symbol::T(4)], &[Symbol::T(7)], &[Symbol::Empty]];
static PARSING_TABLE: [AltId; 210] = [25, 25, 25, 25, 25, 25, 25, 25, 0, 25, 25, 25, 25, 25, 26, 25, 25, 25, 25, 25, 25, 25, 25, 1, 25, 25, 25, 25, 26, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 2, 26, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 3, 26, 26, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 4, 25, 25, 5, 5, 25, 25, 5, 5, 25, 25, 5, 5, 5, 5, 5, 25, 25, 6, 6, 25, 25, 6, 6, 25, 25, 6, 6, 6, 6, 6, 25, 25, 7, 26, 25, 25, 26, 26, 25, 25, 26, 7, 7, 7, 7, 25, 25, 12, 26, 26, 26, 26, 26, 26, 25, 26, 9, 10, 11, 8, 25, 25, 13, 14, 25, 25, 14, 14, 25, 25, 14, 13, 13, 13, 13, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 15, 16, 25, 25, 17, 25, 25, 18, 18, 25, 25, 18, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 19, 25, 25, 20, 25, 25, 25, 26, 26, 25, 24, 24, 21, 22, 24, 24, 23, 25, 24, 24, 24, 24, 24, 25];
static OPCODES: [&[OpCode]; 25] = [&[OpCode::Exit(0), OpCode::NT(2), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::T(6), OpCode::T(13), OpCode::T(8)], &[OpCode::NT(10), OpCode::Exit(2), OpCode::NT(3)], &[OpCode::NT(12), OpCode::NT(5), OpCode::T(0), OpCode::NT(4)], &[OpCode::Exit(4), OpCode::T(13)], &[OpCode::NT(11), OpCode::Exit(5), OpCode::NT(6)], &[OpCode::Exit(6), OpCode::NT(9)], &[OpCode::NT(13), OpCode::NT(8)], &[OpCode::Exit(8), OpCode::T(13)], &[OpCode::Exit(9), OpCode::T(10)], &[OpCode::Exit(10), OpCode::T(11)], &[OpCode::Exit(11), OpCode::T(12)], &[OpCode::Exit(12), OpCode::T(5), OpCode::NT(5), OpCode::T(1)], &[OpCode::Loop(9), OpCode::Exit(13), OpCode::NT(7)], &[OpCode::Exit(14)], &[OpCode::Loop(10), OpCode::Exit(15), OpCode::NT(3)], &[OpCode::Exit(16)], &[OpCode::Loop(11), OpCode::Exit(17), OpCode::NT(6), OpCode::T(2)], &[OpCode::Exit(18)], &[OpCode::Exit(19), OpCode::T(6)], &[OpCode::Exit(20), OpCode::T(6), OpCode::T(9)], &[OpCode::Exit(21), OpCode::T(3)], &[OpCode::Exit(22), OpCode::T(4)], &[OpCode::Exit(23), OpCode::T(7)], &[OpCode::Exit(24)]];
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
    /// `file -> header rules`
    File { header: SynHeader, rules: SynRules },
}
#[derive(Debug)]
pub enum CtxHeader {
    /// `header -> "grammar" Id ";"`
    Header { id: String },
}
#[derive(Debug)]
pub enum CtxRules {
    /// `rules -> rule`
    Rules1 { rule: SynRule },
    /// `rules -> rules rule`
    Rules2 { rules: SynRules, rule: SynRule },
}
#[derive(Debug)]
pub enum CtxRule {
    /// `rule -> rule_name ":" prod ";"`
    Rule1 { rule_name: SynRuleName, prod: SynProd },
    /// `rule -> rule_name ":" prod "EOF" ";"`
    Rule2 { rule_name: SynRuleName, prod: SynProd },
}
#[derive(Debug)]
pub enum CtxRuleName {
    /// `rule_name -> Id`
    RuleName { id: String },
}
#[derive(Debug)]
pub enum CtxProd {
    /// `prod -> prod_term`
    Prod1 { prod_term: SynProdTerm },
    /// `prod -> prod "|" prod_term`
    Prod2 { prod: SynProd, prod_term: SynProdTerm },
}
#[derive(Debug)]
pub enum CtxProdTerm {
    /// `prod_term -> prod_factor*`
    ProdTerm { star: SynProdTerm1 },
}
#[derive(Debug)]
pub enum CtxProdFactor {
    /// `prod_factor -> prod_atom "+"`
    ProdFactor1 { prod_atom: SynProdAtom },
    /// `prod_factor -> prod_atom "?"`
    ProdFactor2 { prod_atom: SynProdAtom },
    /// `prod_factor -> prod_atom "*"`
    ProdFactor3 { prod_atom: SynProdAtom },
    /// `prod_factor -> prod_atom`
    ProdFactor4 { prod_atom: SynProdAtom },
}
#[derive(Debug)]
pub enum CtxProdAtom {
    /// `prod_atom -> Id`
    ProdAtom1 { id: String },
    /// `prod_atom -> Lform`
    ProdAtom2 { lform: String },
    /// `prod_atom -> "<R>"`
    ProdAtom3,
    /// `prod_atom -> "<P>"`
    ProdAtom4,
    /// `prod_atom -> "(" prod ")"`
    ProdAtom5 { prod: SynProd },
}

// NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

// /// User-defined type for `file`
// #[derive(Debug, PartialEq)] pub struct SynFile();
// /// User-defined type for `header`
// #[derive(Debug, PartialEq)] pub struct SynHeader();
// /// User-defined type for `rules`
// #[derive(Debug, PartialEq)] pub struct SynRules();
// /// User-defined type for `rule`
// #[derive(Debug, PartialEq)] pub struct SynRule();
// /// User-defined type for `rule_name`
// #[derive(Debug, PartialEq)] pub struct SynRuleName();
// /// User-defined type for `prod`
// #[derive(Debug, PartialEq)] pub struct SynProd();
// /// User-defined type for `prod_term`
// #[derive(Debug, PartialEq)] pub struct SynProdTerm();
// /// User-defined type for `prod_factor`
// #[derive(Debug, PartialEq)] pub struct SynProdFactor();
// /// User-defined type for `prod_atom`
// #[derive(Debug, PartialEq)] pub struct SynProdAtom();
/// Computed `prod_factor*` array in `prod_term ->  ►► prod_factor* ◄◄ `
#[derive(Debug, PartialEq)]
pub struct SynProdTerm1(pub Vec<SynProdFactor>);

#[derive(Debug)]
enum SynValue { File(SynFile), Header(SynHeader), Rules(SynRules), Rule(SynRule), RuleName(SynRuleName), Prod(SynProd), ProdTerm(SynProdTerm), ProdFactor(SynProdFactor), ProdAtom(SynProdAtom), ProdTerm1(SynProdTerm1) }

impl SynValue {
    fn get_file(self) -> SynFile {
        if let SynValue::File(val) = self { val } else { panic!() }
    }
    fn get_header(self) -> SynHeader {
        if let SynValue::Header(val) = self { val } else { panic!() }
    }
    fn get_rules(self) -> SynRules {
        if let SynValue::Rules(val) = self { val } else { panic!() }
    }
    fn get_rule(self) -> SynRule {
        if let SynValue::Rule(val) = self { val } else { panic!() }
    }
    fn get_rule_name(self) -> SynRuleName {
        if let SynValue::RuleName(val) = self { val } else { panic!() }
    }
    fn get_prod(self) -> SynProd {
        if let SynValue::Prod(val) = self { val } else { panic!() }
    }
    fn get_prod_term(self) -> SynProdTerm {
        if let SynValue::ProdTerm(val) = self { val } else { panic!() }
    }
    fn get_prod_factor(self) -> SynProdFactor {
        if let SynValue::ProdFactor(val) = self { val } else { panic!() }
    }
    fn get_prod_atom(self) -> SynProdAtom {
        if let SynValue::ProdAtom(val) = self { val } else { panic!() }
    }
    fn get_prod_term1(self) -> SynProdTerm1 {
        if let SynValue::ProdTerm1(val) = self { val } else { panic!() }
    }
}

pub trait GramParserListener {
    /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
    /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
    fn check_abort_request(&self) -> bool { false }
    fn get_mut_log(&mut self) -> &mut impl Logger;
    fn exit(&mut self, _file: SynFile) {}
    fn init_file(&mut self) {}
    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile;
    fn init_header(&mut self) {}
    fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader;
    fn init_rules(&mut self) {}
    fn exit_rules(&mut self, _ctx: CtxRules) -> SynRules;
    fn exitloop_rules(&mut self, _rules: &mut SynRules) {}
    fn init_rule(&mut self) {}
    fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule;
    fn init_rule_name(&mut self) {}
    fn exit_rule_name(&mut self, _ctx: CtxRuleName) -> SynRuleName;
    fn init_prod(&mut self) {}
    fn exit_prod(&mut self, _ctx: CtxProd) -> SynProd;
    fn exitloop_prod(&mut self, _prod: &mut SynProd) {}
    fn init_prod_term(&mut self) {}
    fn exit_prod_term(&mut self, _ctx: CtxProdTerm) -> SynProdTerm;
    fn init_prod_factor(&mut self) {}
    fn exit_prod_factor(&mut self, _ctx: CtxProdFactor) -> SynProdFactor;
    fn init_prod_atom(&mut self) {}
    fn exit_prod_atom(&mut self, _ctx: CtxProdAtom) -> SynProdAtom;
}

pub struct Wrapper<T> {
    verbose: bool,
    listener: T,
    stack: Vec<SynValue>,
    max_stack: usize,
    stack_t: Vec<String>,
}

impl<T: GramParserListener> ListenerWrapper for Wrapper<T> {
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
                    1 => self.listener.init_header(),           // header
                    2 => self.listener.init_rules(),            // rules
                    10 => {}                                    // rules_1
                    3 => self.listener.init_rule(),             // rule
                    12 => {}                                    // rule_1
                    4 => self.listener.init_rule_name(),        // rule_name
                    5 => self.listener.init_prod(),             // prod
                    11 => {}                                    // prod_1
                    6 => self.listener.init_prod_term(),        // prod_term
                    9 => self.init_prod_term1(),                // prod_term_1
                    7 => self.listener.init_prod_factor(),      // prod_factor
                    13 => {}                                    // prod_factor_1
                    8 => self.listener.init_prod_atom(),        // prod_atom
                    _ => panic!("unexpected enter nonterminal id: {nt}")
                }
            }
            Call::Loop => {}
            Call::Exit => {
                match alt_id {
                    0 => self.exit_file(),                      // file -> header rules
                    1 => self.exit_header(),                    // header -> grammar Id ;
                    2 => self.inter_rules(),                    // rules -> rule rules_1
                    15 => self.exit_rules1(),                   // rules_1 -> rule rules_1
                    16 => self.exitloop_rules1(),               // rules_1 -> ε
                    19 |                                        // rule_1 -> ;
                    20 => self.exit_rule(alt_id),               // rule_1 -> EOF ;
                 /* 3 */                                        // rule -> rule_name : prod rule_1 (never called)
                    4 => self.exit_rule_name(),                 // rule_name -> Id
                    5 => self.inter_prod(),                     // prod -> prod_term prod_1
                    17 => self.exit_prod1(),                    // prod_1 -> | prod_term prod_1
                    18 => self.exitloop_prod1(),                // prod_1 -> ε
                    6 => self.exit_prod_term(),                 // prod_term -> prod_term_1
                    13 => self.exit_prod_term1(),               // prod_term_1 -> prod_factor prod_term_1
                    14 => {}                                    // prod_term_1 -> ε
                    21 |                                        // prod_factor_1 -> +
                    22 |                                        // prod_factor_1 -> ?
                    23 |                                        // prod_factor_1 -> *
                    24 => self.exit_prod_factor(alt_id),        // prod_factor_1 -> ε
                 /* 7 */                                        // prod_factor -> prod_atom prod_factor_1 (never called)
                    8 |                                         // prod_atom -> Id
                    9 |                                         // prod_atom -> Lform
                    10 |                                        // prod_atom -> <R>
                    11 |                                        // prod_atom -> <P>
                    12 => self.exit_prod_atom(alt_id),          // prod_atom -> ( prod )
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

impl<T: GramParserListener> Wrapper<T> {
    pub fn new(listener: T, verbose: bool) -> Self {
        Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
    }

    pub fn get_listener(&self) -> &T {
        &self.listener
    }

    pub fn get_mut_listener(&mut self) -> &mut T {
        &mut self.listener
    }

    pub fn listener(self) -> T {
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
        let rules = self.stack.pop().unwrap().get_rules();
        let header = self.stack.pop().unwrap().get_header();
        let val = self.listener.exit_file(CtxFile::File { header, rules });
        self.stack.push(SynValue::File(val));
    }

    fn exit_header(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let val = self.listener.exit_header(CtxHeader::Header { id });
        self.stack.push(SynValue::Header(val));
    }

    fn inter_rules(&mut self) {
        let rule = self.stack.pop().unwrap().get_rule();
        let val = self.listener.exit_rules(CtxRules::Rules1 { rule });
        self.stack.push(SynValue::Rules(val));
    }

    fn exit_rules1(&mut self) {
        let rule = self.stack.pop().unwrap().get_rule();
        let rules = self.stack.pop().unwrap().get_rules();
        let val = self.listener.exit_rules(CtxRules::Rules2 { rules, rule });
        self.stack.push(SynValue::Rules(val));
    }

    fn exitloop_rules1(&mut self) {
        let SynValue::Rules(rules) = self.stack.last_mut().unwrap() else { panic!() };
        self.listener.exitloop_rules(rules);
    }

    fn exit_rule(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            19 => {
                let prod = self.stack.pop().unwrap().get_prod();
                let rule_name = self.stack.pop().unwrap().get_rule_name();
                CtxRule::Rule1 { rule_name, prod }
            }
            20 => {
                let prod = self.stack.pop().unwrap().get_prod();
                let rule_name = self.stack.pop().unwrap().get_rule_name();
                CtxRule::Rule2 { rule_name, prod }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_rule")
        };
        let val = self.listener.exit_rule(ctx);
        self.stack.push(SynValue::Rule(val));
    }

    fn exit_rule_name(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let val = self.listener.exit_rule_name(CtxRuleName::RuleName { id });
        self.stack.push(SynValue::RuleName(val));
    }

    fn inter_prod(&mut self) {
        let prod_term = self.stack.pop().unwrap().get_prod_term();
        let val = self.listener.exit_prod(CtxProd::Prod1 { prod_term });
        self.stack.push(SynValue::Prod(val));
    }

    fn exit_prod1(&mut self) {
        let prod_term = self.stack.pop().unwrap().get_prod_term();
        let prod = self.stack.pop().unwrap().get_prod();
        let val = self.listener.exit_prod(CtxProd::Prod2 { prod, prod_term });
        self.stack.push(SynValue::Prod(val));
    }

    fn exitloop_prod1(&mut self) {
        let SynValue::Prod(prod) = self.stack.last_mut().unwrap() else { panic!() };
        self.listener.exitloop_prod(prod);
    }

    fn exit_prod_term(&mut self) {
        let star = self.stack.pop().unwrap().get_prod_term1();
        let val = self.listener.exit_prod_term(CtxProdTerm::ProdTerm { star });
        self.stack.push(SynValue::ProdTerm(val));
    }

    fn init_prod_term1(&mut self) {
        let val = SynProdTerm1(Vec::new());
        self.stack.push(SynValue::ProdTerm1(val));
    }

    fn exit_prod_term1(&mut self) {
        let prod_factor = self.stack.pop().unwrap().get_prod_factor();
        let mut star_it = self.stack.pop().unwrap().get_prod_term1();
        star_it.0.push(prod_factor);
        self.stack.push(SynValue::ProdTerm1(star_it));
    }

    fn exit_prod_factor(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            21 => {
                let prod_atom = self.stack.pop().unwrap().get_prod_atom();
                CtxProdFactor::ProdFactor1 { prod_atom }
            }
            22 => {
                let prod_atom = self.stack.pop().unwrap().get_prod_atom();
                CtxProdFactor::ProdFactor2 { prod_atom }
            }
            23 => {
                let prod_atom = self.stack.pop().unwrap().get_prod_atom();
                CtxProdFactor::ProdFactor3 { prod_atom }
            }
            24 => {
                let prod_atom = self.stack.pop().unwrap().get_prod_atom();
                CtxProdFactor::ProdFactor4 { prod_atom }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_prod_factor")
        };
        let val = self.listener.exit_prod_factor(ctx);
        self.stack.push(SynValue::ProdFactor(val));
    }

    fn exit_prod_atom(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            8 => {
                let id = self.stack_t.pop().unwrap();
                CtxProdAtom::ProdAtom1 { id }
            }
            9 => {
                let lform = self.stack_t.pop().unwrap();
                CtxProdAtom::ProdAtom2 { lform }
            }
            10 => {
                CtxProdAtom::ProdAtom3
            }
            11 => {
                CtxProdAtom::ProdAtom4
            }
            12 => {
                let prod = self.stack.pop().unwrap().get_prod();
                CtxProdAtom::ProdAtom5 { prod }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_prod_atom")
        };
        let val = self.listener.exit_prod_atom(ctx);
        self.stack.push(SynValue::ProdAtom(val));
    }
}

// [gramparser]
// -------------------------------------------------------------------------

pub(crate) mod gramparser_types {

    /// User-defined type for `file`
    #[derive(Debug, PartialEq)] pub struct SynFile();
    /// User-defined type for `header`
    #[derive(Debug, PartialEq)] pub struct SynHeader();
    /// User-defined type for `rules`
    #[derive(Debug, PartialEq)] pub struct SynRules();
    /// User-defined type for `rule`
    #[derive(Debug, PartialEq)] pub struct SynRule();
    /// User-defined type for `rule_name`
    #[derive(Debug, PartialEq)] pub struct SynRuleName(pub String);
    /// User-defined type for `prod`
    #[derive(Debug, PartialEq)] pub struct SynProd(pub usize);
    /// User-defined type for `prod_term`
    #[derive(Debug, PartialEq)] pub struct SynProdTerm(pub usize);
    /// User-defined type for `prod_factor`
    #[derive(Debug, PartialEq)] pub struct SynProdFactor(pub usize);
    /// User-defined type for `prod_atom`
    #[derive(Debug, PartialEq)] pub struct SynProdAtom(pub usize);
}
