// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
// [gramparser]

use gramparser_types::*;
use lexigram_lib::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::{LogMsg, Logger}, parser::{Call, LLParser, ListenerWrapper, OpCode, Terminate}};

const PARSER_NUM_T: usize = 18;
const PARSER_NUM_NT: usize = 14;
static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Colon", Some(":")), ("Lparen", Some("(")), ("Or", Some("|")), ("Plus", Some("+")), ("Question", Some("?")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Sep", Some("/")), ("Star", Some("*")), ("StrLit", None), ("Grammar", Some("grammar")), ("SymEof", Some("EOF")), ("Lform", None), ("Rform", Some("<R>")), ("Pform", Some("<P>")), ("Greedy", Some("<G>")), ("ResolveTag", Some("<resolve>")), ("Id", None)];
static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "header", "rules", "rule", "rule_name", "prod", "prod_alt", "prod_factor", "prod_atom", "prod_alt_1", "rules_1", "prod_1", "rule_1", "prod_factor_1"];
static ALT_VAR: [VarId; 29] = [0, 1, 2, 3, 4, 4, 5, 6, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 13, 13];
static PARSING_TABLE: [AltId; 266] = [29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 0, 29, 29, 29, 29, 29, 29, 29, 30, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 1, 29, 29, 29, 29, 29, 30, 30, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 2, 2, 30, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 3, 3, 30, 30, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 4, 5, 29, 29, 6, 6, 29, 29, 6, 6, 6, 29, 6, 29, 6, 6, 6, 6, 6, 29, 6, 29, 29, 7, 7, 29, 29, 7, 7, 7, 29, 7, 29, 7, 7, 7, 7, 7, 29, 7, 29, 29, 8, 30, 29, 29, 30, 30, 8, 29, 8, 29, 30, 8, 8, 8, 8, 29, 8, 29, 29, 14, 30, 30, 30, 30, 30, 15, 30, 16, 29, 30, 10, 11, 12, 13, 29, 9, 29, 29, 17, 18, 29, 29, 18, 18, 17, 29, 17, 29, 18, 17, 17, 17, 17, 29, 17, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 19, 19, 20, 29, 29, 21, 29, 29, 22, 22, 29, 29, 29, 29, 22, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 23, 29, 29, 29, 29, 24, 29, 29, 29, 29, 30, 30, 30, 29, 28, 28, 25, 26, 28, 28, 28, 27, 28, 29, 28, 28, 28, 28, 28, 29, 28, 29];
static OPCODES: [&[OpCode]; 29] = [&[OpCode::Exit(0), OpCode::NT(2), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::T(6), OpCode::T(17), OpCode::T(10)], &[OpCode::NT(10), OpCode::Exit(2), OpCode::NT(3)], &[OpCode::NT(12), OpCode::NT(5), OpCode::T(0), OpCode::NT(4)], &[OpCode::Exit(4), OpCode::T(17), OpCode::T(16)], &[OpCode::Exit(5), OpCode::T(17)], &[OpCode::NT(11), OpCode::Exit(6), OpCode::NT(6)], &[OpCode::Exit(7), OpCode::NT(9)], &[OpCode::NT(13), OpCode::NT(8)], &[OpCode::Exit(9), OpCode::T(17)], &[OpCode::Exit(10), OpCode::T(12)], &[OpCode::Exit(11), OpCode::T(13)], &[OpCode::Exit(12), OpCode::T(14)], &[OpCode::Exit(13), OpCode::T(15)], &[OpCode::Exit(14), OpCode::T(5), OpCode::NT(5), OpCode::T(1)], &[OpCode::Exit(15), OpCode::T(7)], &[OpCode::Exit(16), OpCode::T(9)], &[OpCode::Loop(9), OpCode::Exit(17), OpCode::NT(7)], &[OpCode::Exit(18)], &[OpCode::Loop(10), OpCode::Exit(19), OpCode::NT(3)], &[OpCode::Exit(20)], &[OpCode::Loop(11), OpCode::Exit(21), OpCode::NT(6), OpCode::T(2)], &[OpCode::Exit(22)], &[OpCode::Exit(23), OpCode::T(6)], &[OpCode::Exit(24), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(25), OpCode::T(3)], &[OpCode::Exit(26), OpCode::T(4)], &[OpCode::Exit(27), OpCode::T(8)], &[OpCode::Exit(28)]];
static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
static START_SYMBOL: VarId = 0;

pub fn build_parser() -> LLParser<'static> {{
    let symbol_table = FixedSymTable::new(
        SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
        SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
    );
    LLParser::new(
        PARSER_NUM_NT, PARSER_NUM_T + 1,
        &ALT_VAR,
        Vec::new(),
        OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
        INIT_OPCODES.to_vec(),
        &PARSING_TABLE,
        symbol_table,
        START_SYMBOL
    )
}}

#[derive(Debug)]
pub enum CtxFile {
    /// `file -> header rules`
    V1 { header: SynHeader, rules: SynRules },
}
#[derive(Debug)]
pub enum CtxHeader {
    /// `header -> "grammar" Id ";"`
    V1 { id: String },
}
#[derive(Debug)]
pub enum CtxRules {
    /// `rules -> rule`
    V1 { rule: SynRule },
    /// `rules -> rules rule`
    V2 { rules: SynRules, rule: SynRule },
}
#[derive(Debug)]
pub enum CtxRule {
    /// `rule -> rule_name ":" prod "EOF" ";"`
    V1 { rule_name: SynRuleName, prod: SynProd },
    /// `rule -> rule_name ":" prod ";"`
    V2 { rule_name: SynRuleName, prod: SynProd },
}
#[derive(Debug)]
pub enum CtxRuleName {
    /// `rule_name -> "<resolve>" Id`
    V1 { id: String },
    /// `rule_name -> Id`
    V2 { id: String },
}
#[derive(Debug)]
pub enum CtxProd {
    /// `prod -> prod_alt`
    V1 { prod_alt: SynProdAlt },
    /// `prod -> prod "|" prod_alt`
    V2 { prod: SynProd, prod_alt: SynProdAlt },
}
#[derive(Debug)]
pub enum CtxProdAlt {
    /// `prod_alt -> prod_factor*`
    V1 { star: SynProdAlt1 },
}
#[derive(Debug)]
pub enum CtxProdFactor {
    /// `prod_factor -> prod_atom "+"`
    V1 { prod_atom: SynProdAtom },
    /// `prod_factor -> prod_atom "*"`
    V2 { prod_atom: SynProdAtom },
    /// `prod_factor -> prod_atom "?"`
    V3 { prod_atom: SynProdAtom },
    /// `prod_factor -> prod_atom`
    V4 { prod_atom: SynProdAtom },
}
#[derive(Debug)]
pub enum CtxProdAtom {
    /// `prod_atom -> Id`
    V1 { id: String },
    /// `prod_atom -> Lform`
    V2 { lform: String },
    /// `prod_atom -> "<R>"`
    V3,
    /// `prod_atom -> "<P>"`
    V4,
    /// `prod_atom -> "<G>"`
    V5,
    /// `prod_atom -> "(" prod ")"`
    V6 { prod: SynProd },
    /// `prod_atom -> "/"`
    V7,
    /// `prod_atom -> StrLit`
    V8 { strlit: String },
}

/// Computed `prod_factor*` array in `prod_alt ->  ►► prod_factor* ◄◄ `
#[derive(Debug, PartialEq)]
pub struct SynProdAlt1(pub Vec<SynProdFactor>);

#[derive(Debug)]
enum EnumSynValue { File(SynFile), Header(SynHeader), Rules(SynRules), Rule(SynRule), RuleName(SynRuleName), Prod(SynProd), ProdAlt(SynProdAlt), ProdFactor(SynProdFactor), ProdAtom(SynProdAtom), ProdAlt1(SynProdAlt1) }

impl EnumSynValue {
    fn get_file(self) -> SynFile {
        if let EnumSynValue::File(val) = self { val } else { panic!() }
    }
    fn get_header(self) -> SynHeader {
        if let EnumSynValue::Header(val) = self { val } else { panic!() }
    }
    fn get_rules(self) -> SynRules {
        if let EnumSynValue::Rules(val) = self { val } else { panic!() }
    }
    fn get_rule(self) -> SynRule {
        if let EnumSynValue::Rule(val) = self { val } else { panic!() }
    }
    fn get_rule_name(self) -> SynRuleName {
        if let EnumSynValue::RuleName(val) = self { val } else { panic!() }
    }
    fn get_prod(self) -> SynProd {
        if let EnumSynValue::Prod(val) = self { val } else { panic!() }
    }
    fn get_prod_alt(self) -> SynProdAlt {
        if let EnumSynValue::ProdAlt(val) = self { val } else { panic!() }
    }
    fn get_prod_factor(self) -> SynProdFactor {
        if let EnumSynValue::ProdFactor(val) = self { val } else { panic!() }
    }
    fn get_prod_atom(self) -> SynProdAtom {
        if let EnumSynValue::ProdAtom(val) = self { val } else { panic!() }
    }
    fn get_prod_alt1(self) -> SynProdAlt1 {
        if let EnumSynValue::ProdAlt1(val) = self { val } else { panic!() }
    }
}

pub trait GramParserListener {
    /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
    /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
    fn check_abort_request(&self) -> Terminate { Terminate::None }
    fn get_log_mut(&mut self) -> &mut impl Logger;
    #[allow(unused_variables)]
    fn handle_msg(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {
        self.get_log_mut().add(msg);
    }
    #[allow(unused_variables)]
    fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
    #[allow(unused_variables)]
    fn exit(&mut self, file: SynFile, span: PosSpan) {}
    #[allow(unused_variables)]
    fn abort(&mut self, terminate: Terminate) {}
    fn init_file(&mut self) {}
    fn exit_file(&mut self, ctx: CtxFile, spans: Vec<PosSpan>) -> SynFile;
    fn init_header(&mut self) {}
    fn exit_header(&mut self, ctx: CtxHeader, spans: Vec<PosSpan>) -> SynHeader;
    fn init_rules(&mut self) {}
    fn exit_rules(&mut self, ctx: CtxRules, spans: Vec<PosSpan>) -> SynRules;
    #[allow(unused_variables)]
    fn exitloop_rules(&mut self, rules: &mut SynRules) {}
    fn init_rule(&mut self) {}
    fn exit_rule(&mut self, ctx: CtxRule, spans: Vec<PosSpan>) -> SynRule;
    fn init_rule_name(&mut self) {}
    fn exit_rule_name(&mut self, ctx: CtxRuleName, spans: Vec<PosSpan>) -> SynRuleName;
    fn init_prod(&mut self) {}
    fn exit_prod(&mut self, ctx: CtxProd, spans: Vec<PosSpan>) -> SynProd;
    #[allow(unused_variables)]
    fn exitloop_prod(&mut self, prod: &mut SynProd) {}
    fn init_prod_alt(&mut self) {}
    fn exit_prod_alt(&mut self, ctx: CtxProdAlt, spans: Vec<PosSpan>) -> SynProdAlt;
    fn init_prod_factor(&mut self) {}
    fn exit_prod_factor(&mut self, ctx: CtxProdFactor, spans: Vec<PosSpan>) -> SynProdFactor;
    fn init_prod_atom(&mut self) {}
    fn exit_prod_atom(&mut self, ctx: CtxProdAtom, spans: Vec<PosSpan>) -> SynProdAtom;
}

pub struct Wrapper<T> {
    verbose: bool,
    listener: T,
    stack: Vec<EnumSynValue>,
    max_stack: usize,
    stack_t: Vec<String>,
    stack_span: Vec<PosSpan>,
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
                if matches!(nt, 9) {
                    self.stack_span.push(PosSpan::empty());
                }
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
                    6 => self.listener.init_prod_alt(),         // prod_alt
                    9 => self.init_prod_alt1(),                 // prod_alt_1
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
                    1 => self.exit_header(),                    // header -> "grammar" Id ";"
                    2 => self.inter_rules(),                    // rules -> rule rules_1
                    19 => self.exit_rules1(),                   // rules_1 -> rule rules_1
                    20 => self.exitloop_rules1(),               // rules_1 -> ε
                    23 |                                        // rule_1 -> ";"
                    24 => self.exit_rule(alt_id),               // rule_1 -> "EOF" ";"
                 /* 3 */                                        // rule -> rule_name ":" prod rule_1 (never called)
                    4 |                                         // rule_name -> "<resolve>" Id
                    5 => self.exit_rule_name(alt_id),           // rule_name -> Id
                    6 => self.inter_prod(),                     // prod -> prod_alt prod_1
                    21 => self.exit_prod1(),                    // prod_1 -> "|" prod_alt prod_1
                    22 => self.exitloop_prod1(),                // prod_1 -> ε
                    7 => self.exit_prod_alt(),                  // prod_alt -> prod_alt_1
                    17 => self.exit_prod_alt1(),                // prod_alt_1 -> prod_factor prod_alt_1
                    18 => {}                                    // prod_alt_1 -> ε
                    25 |                                        // prod_factor_1 -> "+"
                    26 |                                        // prod_factor_1 -> "?"
                    27 |                                        // prod_factor_1 -> "*"
                    28 => self.exit_prod_factor(alt_id),        // prod_factor_1 -> ε
                 /* 8 */                                        // prod_factor -> prod_atom prod_factor_1 (never called)
                    9 |                                         // prod_atom -> Id
                    10 |                                        // prod_atom -> Lform
                    11 |                                        // prod_atom -> "<R>"
                    12 |                                        // prod_atom -> "<P>"
                    13 |                                        // prod_atom -> "<G>"
                    14 |                                        // prod_atom -> "(" prod ")"
                    15 |                                        // prod_atom -> "/"
                    16 => self.exit_prod_atom(alt_id),          // prod_atom -> StrLit
                    _ => panic!("unexpected exit alternative id: {alt_id}")
                }
            }
            Call::End(terminate) => {
                match terminate {
                    Terminate::None => {
                        let val = self.stack.pop().unwrap().get_file();
                        let span = self.stack_span.pop().unwrap();
                        self.listener.exit(val, span);
                    }
                    Terminate::Abort | Terminate::Conclude => self.listener.abort(terminate),
                }
            }
        }
        self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
        if self.verbose {
            println!("> stack_t:   {}", self.stack_t.join(", "));
            println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).collect::<Vec<_>>().join(", "));
        }
    }

    fn check_abort_request(&self) -> Terminate {
        self.listener.check_abort_request()
    }

    fn abort(&mut self) {
        self.stack.clear();
        self.stack_span.clear();
        self.stack_t.clear();
    }

    fn get_log_mut(&mut self) -> &mut impl Logger {
        self.listener.get_log_mut()
    }

    fn report(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {
        self.listener.handle_msg(span_opt, msg);
    }

    fn push_span(&mut self, span: PosSpan) {
        self.stack_span.push(span);
    }

    fn is_stack_empty(&self) -> bool {
        self.stack.is_empty()
    }

    fn is_stack_t_empty(&self) -> bool {
        self.stack_t.is_empty()
    }

    fn is_stack_span_empty(&self) -> bool {
        self.stack_span.is_empty()
    }

    fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
        self.listener.intercept_token(token, text, span)
    }
}

impl<T: GramParserListener> Wrapper<T> {
    pub fn new(listener: T, verbose: bool) -> Self {
        Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new(), stack_span: Vec::new() }
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

    fn exit_file(&mut self) {
        let rules = self.stack.pop().unwrap().get_rules();
        let header = self.stack.pop().unwrap().get_header();
        let ctx = CtxFile::V1 { header, rules };
        let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_file(ctx, spans);
        self.stack.push(EnumSynValue::File(val));
    }

    fn exit_header(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let ctx = CtxHeader::V1 { id };
        let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_header(ctx, spans);
        self.stack.push(EnumSynValue::Header(val));
    }

    fn inter_rules(&mut self) {
        let rule = self.stack.pop().unwrap().get_rule();
        let ctx = CtxRules::V1 { rule };
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_rules(ctx, spans);
        self.stack.push(EnumSynValue::Rules(val));
    }

    fn exit_rules1(&mut self) {
        let rule = self.stack.pop().unwrap().get_rule();
        let rules = self.stack.pop().unwrap().get_rules();
        let ctx = CtxRules::V2 { rules, rule };
        let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_rules(ctx, spans);
        self.stack.push(EnumSynValue::Rules(val));
    }

    fn exitloop_rules1(&mut self) {
        let EnumSynValue::Rules(rules) = self.stack.last_mut().unwrap() else { panic!() };
        self.listener.exitloop_rules(rules);
    }

    fn exit_rule(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            23 => {
                let prod = self.stack.pop().unwrap().get_prod();
                let rule_name = self.stack.pop().unwrap().get_rule_name();
                (4, CtxRule::V2 { rule_name, prod })
            }
            24 => {
                let prod = self.stack.pop().unwrap().get_prod();
                let rule_name = self.stack.pop().unwrap().get_rule_name();
                (5, CtxRule::V1 { rule_name, prod })
            }
            _ => panic!("unexpected alt id {alt_id} in method exit_rule")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_rule(ctx, spans);
        self.stack.push(EnumSynValue::Rule(val));
    }

    fn exit_rule_name(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            4 => {
                let id = self.stack_t.pop().unwrap();
                (2, CtxRuleName::V1 { id })
            }
            5 => {
                let id = self.stack_t.pop().unwrap();
                (1, CtxRuleName::V2 { id })
            }
            _ => panic!("unexpected alt id {alt_id} in method exit_rule_name")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_rule_name(ctx, spans);
        self.stack.push(EnumSynValue::RuleName(val));
    }

    fn inter_prod(&mut self) {
        let prod_alt = self.stack.pop().unwrap().get_prod_alt();
        let ctx = CtxProd::V1 { prod_alt };
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_prod(ctx, spans);
        self.stack.push(EnumSynValue::Prod(val));
    }

    fn exit_prod1(&mut self) {
        let prod_alt = self.stack.pop().unwrap().get_prod_alt();
        let prod = self.stack.pop().unwrap().get_prod();
        let ctx = CtxProd::V2 { prod, prod_alt };
        let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_prod(ctx, spans);
        self.stack.push(EnumSynValue::Prod(val));
    }

    fn exitloop_prod1(&mut self) {
        let EnumSynValue::Prod(prod) = self.stack.last_mut().unwrap() else { panic!() };
        self.listener.exitloop_prod(prod);
    }

    fn exit_prod_alt(&mut self) {
        let star = self.stack.pop().unwrap().get_prod_alt1();
        let ctx = CtxProdAlt::V1 { star };
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_prod_alt(ctx, spans);
        self.stack.push(EnumSynValue::ProdAlt(val));
    }

    fn init_prod_alt1(&mut self) {
        let val = SynProdAlt1(Vec::new());
        self.stack.push(EnumSynValue::ProdAlt1(val));
    }

    fn exit_prod_alt1(&mut self) {
        let prod_factor = self.stack.pop().unwrap().get_prod_factor();
        let Some(EnumSynValue::ProdAlt1(SynProdAlt1(star_acc))) = self.stack.last_mut() else {
            panic!("expected SynProdAlt1 item on wrapper stack");
        };
        star_acc.push(prod_factor);
        let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
    }

    fn exit_prod_factor(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            25 => {
                let prod_atom = self.stack.pop().unwrap().get_prod_atom();
                (2, CtxProdFactor::V1 { prod_atom })
            }
            26 => {
                let prod_atom = self.stack.pop().unwrap().get_prod_atom();
                (2, CtxProdFactor::V3 { prod_atom })
            }
            27 => {
                let prod_atom = self.stack.pop().unwrap().get_prod_atom();
                (2, CtxProdFactor::V2 { prod_atom })
            }
            28 => {
                let prod_atom = self.stack.pop().unwrap().get_prod_atom();
                (1, CtxProdFactor::V4 { prod_atom })
            }
            _ => panic!("unexpected alt id {alt_id} in method exit_prod_factor")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_prod_factor(ctx, spans);
        self.stack.push(EnumSynValue::ProdFactor(val));
    }

    fn exit_prod_atom(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            9 => {
                let id = self.stack_t.pop().unwrap();
                (1, CtxProdAtom::V1 { id })
            }
            10 => {
                let lform = self.stack_t.pop().unwrap();
                (1, CtxProdAtom::V2 { lform })
            }
            11 => {
                (1, CtxProdAtom::V3)
            }
            12 => {
                (1, CtxProdAtom::V4)
            }
            13 => {
                (1, CtxProdAtom::V5)
            }
            14 => {
                let prod = self.stack.pop().unwrap().get_prod();
                (3, CtxProdAtom::V6 { prod })
            }
            15 => {
                (1, CtxProdAtom::V7)
            }
            16 => {
                let strlit = self.stack_t.pop().unwrap();
                (1, CtxProdAtom::V8 { strlit })
            }
            _ => panic!("unexpected alt id {alt_id} in method exit_prod_atom")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_prod_atom(ctx, spans);
        self.stack.push(EnumSynValue::ProdAtom(val));
    }
}

// [gramparser]
// -------------------------------------------------------------------------

pub(crate) mod gramparser_types {
    use lexigram_lib::VarId;

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
    #[derive(Debug, PartialEq)] pub struct SynProd(pub usize, pub Option<VarId>);
    /// User-defined type for `prod_term`
    #[derive(Debug, PartialEq)] pub struct SynProdAlt(pub usize, pub Option<VarId>);
    /// User-defined type for `prod_factor`
    #[derive(Debug, PartialEq)] pub struct SynProdFactor(pub usize, pub Option<VarId>);
    /// User-defined type for `prod_atom`
    #[derive(Debug, PartialEq)] pub struct SynProdAtom(pub usize, pub Option<VarId>);
}
