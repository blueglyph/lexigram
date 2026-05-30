// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use std::io::Cursor;
use lexi_gram::lexigram_lib::lexigram_core;
use listener_types::*;
use config_lexer::build_lexer;
use config_parser::*;
use lexi_gram::options::Options;
use listener::Listener;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, TokenSpliterator};
use lexigram_core::log::{BufLog, Logger, LogStatus};
use lexigram_core::parser::LLParser;

const VERBOSE_WRAPPER: bool = false;

pub struct ConfigParser<'l, 'p, 'ls> {
    lexer: Lexer<'l, Cursor<&'l str>>,
    parser: LLParser<'p>,
    wrapper: Option<Wrapper<Listener<'ls>>>,
}

#[derive(Clone, Debug)]
pub struct ConfigResult {
    pub options: Options,
    pub log: BufLog,
}

impl<'l, 'ls: 'l> ConfigParser<'l, '_, 'ls> {
    /// Creates a new parser
    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        ConfigParser { lexer, parser, wrapper: None }
    }

    /// Parses a text.
    ///
    /// On success, returns
    /// * `log`, a `BufLog` object.
    ///
    /// On failure, returns the log with the error messages.
    pub fn parse(&mut self, text: &'ls str) -> Result<ConfigResult, BufLog> {
        self.wrapper = Some(Wrapper::new(Listener::new(), VERBOSE_WRAPPER));
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        self.wrapper.as_mut().unwrap().get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().keep_channel0();
        let result = self.parser.parse_stream(self.wrapper.as_mut().unwrap(), tokens);
        let Listener { mut log, options, .. } = self.wrapper.take().unwrap().give_listener();
        if let Err(e) = result {
            log.add_error(e.to_string());
        }
         if log.has_no_errors() {
            Ok(ConfigResult { options, log })
        } else {
            Err(log)
        }
    }
}

mod listener {
    use std::collections::HashMap;
    use std::str::FromStr;
    use lexi_gram::{gencode, genspec};
    use lexi_gram::lexigram_lib::CollectJoin;
    use lexi_gram::lexigram_lib::lexigram_core::text_span::{GetLine, GetTextSpan};
    use lexi_gram::lexigram_lib::parsergen::NTValue;
    use lexi_gram::options::{Options, Specification};
    use lexigram_core::lexer::PosSpan;
    use lexigram_core::log::Logger;
    use super::*;

    pub(super) struct Listener<'ls> {
        pub options: Options,
        pub log: BufLog,
        lines: Option<Vec<&'ls str>>,
        consts: HashMap<String, SynValue>,
        lexer_indent: Option<usize>,
        parser_indent: Option<usize>,
        // types_indent: Option<usize>,
        // listener_indent: Option<usize>,
    }

    impl<'ls> Listener<'ls> {
        pub fn new() -> Self {
            Listener {
                options: Options::default(),
                log: BufLog::new(),
                lines: None,
                consts: HashMap::new(),
                lexer_indent: None,
                parser_indent: None,
                // types_indent: None,
                // listener_indent: None,
            }
        }

        pub fn attach_lines(&mut self, lines: Vec<&'ls str>) {
            self.lines = Some(lines);
        }

        fn values_to_strings(&mut self, values: Vec<SynValue>, spans: &PosSpan) -> Result<Vec<String>, ()> {
            let values_len = values.len();
            let strings = values.into_iter().enumerate().filter_map(|(i, v)| {
                if let SynValue::Str(s) = v {
                    Some(s)
                } else {
                    self.log.add_error(format!("at {}, item #{} isn't a string", spans, i + 1));
                    None
                }
            }).to_vec();
            if strings.len() == values_len {
                Ok(strings)
            } else {
                Err(())
            }
        }
    }

    impl<'ls> GetLine for Listener<'ls> {
        fn get_line(&self, n: usize) -> &str {
            self.lines.as_ref().unwrap()[n - 1]
        }
    }

    #[allow(unused)]
    impl ConfigListener for Listener<'_> {
        fn get_log_mut(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn exit(&mut self, span: PosSpan) {
            if let Some(indent) = self.lexer_indent { self.options.lexer_indent = indent };
            if let Some(indent) = self.parser_indent { self.options.parser_indent = indent };
            // if let Some(indent) = self.types_indent { self.options.types_indent = indent };
            // if let Some(indent) = self.listener_indent { self.options.listener_indent = indent };
        }

        fn exit_i_def(&mut self, ctx: CtxIDef, spans: Vec<PosSpan>) {
            // `<L> "def" Id "=" value ";"` iteration in `definitions -> ( ►► <L> "def" Id "=" value ";" ◄◄ )*`
            let CtxIDef::V1 { id, value } = ctx;
            if self.consts.contains_key(&id) {
                self.log.add_error(format!("at {}, redefined constant {id}", spans[4]));
            } else if value != SynValue::Error {
                self.consts.insert(id, value);
            }
        }

        fn exit_lexer(&mut self, ctx: CtxLexer, spans: Vec<PosSpan>) {
            // lexer -> "lexer" "{" io_options "}"
            let CtxLexer::V1 { io_options } = ctx;
            if io_options.is_combined {
                self.options.parser_spec = io_options.spec.clone();
            }
            self.options.lexer_spec = io_options.spec;
            self.options.lexer_code = io_options.code;
            self.options.lexer_headers.extend(io_options.headers);
            self.lexer_indent = io_options.indent_opt;
        }

        fn exit_parser(&mut self, ctx: CtxParser, spans: Vec<PosSpan>) {
            match ctx {
                // parser -> "parser" "{" io_options "}"
                CtxParser::V1 { io_options } => {
                    if io_options.is_combined {
                        self.log.add_error(
                            format!("at {}, 'combined' can only be used in 'lexer' options", spans[2]));
                    } else {
                        if io_options.spec.is_none() && self.options.parser_spec.is_none() {
                            self.log.add_error(format!("at {}, undefined grammar location", spans[2]));
                        } else if !io_options.spec.is_none() && !self.options.parser_spec.is_none() {
                            self.log.add_error(
                                format!("at {}, redefined grammar location ('combined' in lexer)", spans[2]));
                        }
                    }
                    if !io_options.spec.is_none() {
                        self.options.parser_spec = io_options.spec;
                    }
                    self.options.parser_code = io_options.code;
                    self.options.parser_headers.extend(io_options.headers);
                    self.parser_indent = io_options.indent_opt;
                }
                // parser -> ε
                CtxParser::V2 => {
                    if !self.options.parser_spec.is_none() {
                        self.log.add_error(
                            "combined lexicon/grammar, but missing parser code location".to_string());
                    }
                }
            }
        }

        fn exit_options(&mut self, ctx: CtxOptions, spans: Vec<PosSpan>) {
            match ctx {
                // options -> "options" "{" global_options "}"
                CtxOptions::V1 { global_options } => {
                    self.options.lexer_headers.extend(global_options.headers.clone());
                    self.options.parser_headers.extend(global_options.headers);
                    if let Some(indent) = global_options.indent_opt {
                        self.options.lexer_indent = indent;
                        self.options.parser_indent = indent;
                        // self.options.types_indent = indent;
                        // self.options.listener_indent = indent;
                    }
                    self.options.libs.extend(global_options.libs);
                    if let Some(nt_value) = global_options.nt_value_opt {
                        self.options.nt_value = nt_value;
                    }
                    if let Some(spans_opt) = global_options.spans_opt {
                        self.options.gen_span_params = spans_opt;
                    }
                }
                // options -> ε
                CtxOptions::V2 => {}
            }
        }

        fn exit_io_options(&mut self, ctx: CtxIoOptions, spans: Vec<PosSpan>) -> SynIoOptions {
            // io_options -> io_option (<L> "," io_option)*
            let CtxIoOptions::V1 { star } = ctx;
            star
        }

        fn init_i_io_opt(&mut self, ctx: InitCtxIIoOpt, spans: Vec<PosSpan>) -> SynIIoOpt {
            // value of `io_option` before `<L> "," io_option` iteration in `io_options -> io_option ( ►► <L> "," io_option ◄◄ )*`
            let InitCtxIIoOpt::V1 { io_option } = ctx;
            let mut acc = SynIIoOpt::new();
            if let Err(e) = acc.fold(io_option) {
                self.log.add_error(format!("at {}, {e}", spans[0]));
            }
            acc
        }

        fn exit_i_io_opt(&mut self, acc: &mut SynIIoOpt, ctx: CtxIIoOpt, spans: Vec<PosSpan>) {
            // `<L> "," io_option` iteration in `io_options -> io_option ( ►► <L> "," io_option ◄◄ )*`
            let CtxIIoOpt::V1 { io_option } = ctx;
            if let Err(e) = acc.fold(io_option) {
                self.log.add_error(format!("at {}, {e}", spans[2]));
            }
        }

        fn exit_io_option(&mut self, ctx: CtxIoOption, spans: Vec<PosSpan>) -> SynIoOption {
            fn get_spec(value: SynValue, tag_opt: SynTagOpt) -> Result<Specification, &'static str> {
                match value {
                    SynValue::Error => Ok(genspec!(none)),
                    SynValue::Bool(_)
                    | SynValue::Num(_)
                    | SynValue::CodeStdout => Err("string"),
                    SynValue::Str(s) => {
                        if let Some(tag) = tag_opt {
                            Ok(genspec!(filename: s, tag: tag))
                        } else {
                            Ok(genspec!(filename: s))
                        }
                    }
                }
            }

            match ctx {
                // io_option -> "combined" ":" value tag_opt
                CtxIoOption::V1 { value, tag_opt } => {
                    match get_spec(value, tag_opt) {
                        Ok(spec) => SynIoOption::Combined(spec),
                        Err(expected) => {
                            self.log.add_error(format!("at {}, expected {expected}", spans[2]));
                            SynIoOption::Error
                        }
                    }
                }
                // io_option -> "input" ":" value tag_opt
                CtxIoOption::V2 { value, tag_opt } => {
                    match get_spec(value, tag_opt) {
                        Ok(spec) => SynIoOption::Spec(spec),
                        Err(expected) => {
                            self.log.add_error(format!("at {}, expected {expected}", spans[2]));
                            SynIoOption::Error
                        }
                    }
                }
                // io_option -> "output" ":" value tag_opt
                CtxIoOption::V3 { value, tag_opt } => {
                    match value {
                        SynValue::Error => SynIoOption::Error,
                        SynValue::Bool(_)
                        | SynValue::Num(_) => {
                            self.log.add_error(format!("at {}, expected string or stdout", spans[2]));
                            SynIoOption::Error
                        }
                        SynValue::Str(s) => {
                            if let Some(tag) = tag_opt {
                                SynIoOption::Code(gencode!(filename: s, tag: tag))
                            } else {
                                SynIoOption::Code(gencode!(filename: s))
                            }
                        }
                        SynValue::CodeStdout => SynIoOption::Code(gencode!(stdout)),
                    }
                }
                // io_option -> "indent" ":" value
                CtxIoOption::V4 { value } => {
                    if let SynValue::Num(indent) = value {
                        SynIoOption::Indent(indent)
                    } else {
                        if value != SynValue::Error {
                            self.log.add_error(format!("at {}, expected number instead of {value:?}", spans[2]));
                        }
                        SynIoOption::Error
                    }
                }
                // io_option -> "headers" ":" "{" value ("," value)* "}"
                CtxIoOption::V5 { star: SynIoOption1(values) } => {
                    match self.values_to_strings(values, &spans[3]) {
                        Ok(headers) => SynIoOption::Headers(headers),
                        Err(()) => SynIoOption::Error,
                    }
                }
            }
        }

        fn exit_tag_opt(&mut self, ctx: CtxTagOpt, spans: Vec<PosSpan>) -> SynTagOpt {
            match ctx {
                // tag_opt -> "[" value "]"
                CtxTagOpt::V1 { value } => {
                    if let SynValue::Str(s) = value {
                        Some(s)
                    } else {
                        self.log.add_error(format!("at {}, expected a string", spans[1]));
                        None
                    }
                }
                // tag_opt -> ε
                CtxTagOpt::V2 => None,
            }
        }

        fn exit_global_options(&mut self, ctx: CtxGlobalOptions, spans: Vec<PosSpan>) -> SynGlobalOptions {
            // global_options -> global_option (<L> "," global_option)*
            let CtxGlobalOptions::V1 { star } = ctx;
            star
        }

        fn init_i_global_opt(&mut self, ctx: InitCtxIGlobalOpt, spans: Vec<PosSpan>) -> SynIGlobalOpt {
            // value of `global_option` before `<L> "," global_option` iteration in `global_options -> global_option ( ►► <L> "," global_option ◄◄ )*`
            let InitCtxIGlobalOpt::V1 { global_option } = ctx;
            let mut acc = SynGlobalOptions::new();
            acc.fold(global_option);
            acc
        }

        fn exit_i_global_opt(&mut self, acc: &mut SynIGlobalOpt, ctx: CtxIGlobalOpt, spans: Vec<PosSpan>) {
            // `<L> "," global_option` iteration in `global_options -> global_option ( ►► <L> "," global_option ◄◄ )*`
            let CtxIGlobalOpt::V1 { global_option } = ctx;
            if let Err(e) = acc.fold(global_option) {
                self.log.add_error(format!("at {}, {e}", spans[2]));
            }
        }

        fn exit_global_option(&mut self, ctx: CtxGlobalOption, spans: Vec<PosSpan>) -> SynGlobalOption {
            match ctx {
                // global_option -> "headers" ":" "{" value ("," value)* "}"
                CtxGlobalOption::V1 { star: SynGlobalOption1(values) } => {
                    match self.values_to_strings(values, &spans[3]) {
                        Ok(headers) => SynGlobalOption::Headers(headers),
                        Err(()) => SynGlobalOption::Error,
                    }
                }
                // global_option -> "indent" ":" value
                CtxGlobalOption::V2 { value } => {
                    if let SynValue::Num(indent) = value {
                        SynGlobalOption::Indent(indent)
                    } else {
                        if value != SynValue::Error {
                            self.log.add_error(format!("at {}, expected number instead of {value:?}", spans[2]));
                        }
                        SynGlobalOption::Error
                    }
                }
                // global_option -> "libs" ":" "{" value ("," value)* "}"
                CtxGlobalOption::V3 { star: SynGlobalOption2(values) } => {
                    match self.values_to_strings(values, &spans[3]) {
                        Ok(libs) => SynGlobalOption::Libs(libs),
                        Err(()) => SynGlobalOption::Error,
                    }
                }
                // global_option -> "nt-value" ":" nt_value
                CtxGlobalOption::V4 { nt_value } => SynGlobalOption::NTValue(nt_value),
                // global_option -> "spans" ":" value
                CtxGlobalOption::V5 { value } => {
                    if let SynValue::Bool(flag) = value {
                        SynGlobalOption::Spans(flag)
                    } else {
                        self.log.add_error(format!("at {}, expected boolean instead of {value:?}", spans[2]));
                        SynGlobalOption::Error
                    }
                }
            }
        }

        fn exit_value(&mut self, ctx: CtxValue, spans: Vec<PosSpan>) -> SynValue {
            match ctx {
                // value -> BoolLiteral
                CtxValue::V1 { boolliteral } => SynValue::Bool(boolliteral == "true"),
                // value -> NumLiteral
                CtxValue::V2 { numliteral } => {
                    match usize::from_str(&numliteral) {
                        Ok(v) => SynValue::Num(v),
                        Err(e) => {
                            self.log.add_error(format!("at {}, {e}", spans[0]));
                            SynValue::Error
                        }
                    }
                }
                // value -> StrLiteral
                CtxValue::V3 { mut strliteral } => {
                    strliteral.remove(0);
                    strliteral.pop();
                    SynValue::Str(strliteral)
                }
                // value -> Id
                CtxValue::V4 { id } => {
                    if let Some(v) = self.consts.get(&id) {
                        v.clone()
                    } else {
                        // self.log.add_error(format!("at {}, {id} is not defined", spans[0]));
                        let text = self.annotate_text(&spans[0]);
                        self.log.add_error(format!("{id} is not defined:\n\n{text}\n"));
                        SynValue::Error
                    }
                }
                // value -> "stdout"
                CtxValue::V5 => SynValue::CodeStdout,
            }
        }

        fn exit_nt_value(&mut self, ctx: CtxNtValue, spans: Vec<PosSpan>) -> SynNtValue {
            match ctx {
                // nt_value -> "default"
                CtxNtValue::V1 => NTValue::Default,
                // nt_value -> "none"
                CtxNtValue::V2 => NTValue::None,
                // nt_value -> "parents"
                CtxNtValue::V3 => NTValue::Parents,
                // nt_value -> "set" "{" value ("," value)* "}"
                CtxNtValue::V4 { star: SynNtValue1(values) } => {
                    match self.values_to_strings(values, &spans[2]) {
                        Ok(names) => NTValue::SetNames(names),
                        Err(()) => NTValue::SetNames(vec![]),
                    }
                }
            }
        }
    }
}

#[allow(unused)]
mod listener_types {
    use lexi_gram::lexigram_lib::parsergen::NTValue;
    use lexi_gram::options::{CodeLocation, Specification};

    /// User-defined type for `io_options`
    #[derive(Clone, PartialEq, Debug)]
    pub struct SynIoOptions {
        pub is_combined: bool,
        pub spec: Specification,
        pub code: CodeLocation,
        pub indent_opt: Option<usize>,
        pub headers: Vec<String>,
    }

    impl Default for SynIoOptions {
        fn default() -> Self {
            SynIoOptions {
                is_combined: false,
                spec: Specification::None,
                code: CodeLocation::None,
                indent_opt: None,
                headers: vec![],
            }
        }
    }

    impl SynIoOptions {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn fold(&mut self, io_option: SynIoOption) -> Result<(), &str> {
            match io_option {
                SynIoOption::Error => {}
                SynIoOption::Combined(spec) => {
                    if self.spec.is_none() {
                        self.is_combined = true;
                        self.spec = spec;
                    } else {
                        return Err("more than one specification");
                    }
                }
                SynIoOption::Spec(spec) => {
                    if self.spec.is_none() {
                        self.spec = spec;
                    } else {
                        return Err("more than one specification");
                    }
                }
                SynIoOption::Code(code) => {
                    if self.code.is_none() {
                        self.code = code;
                    } else {
                        return Err("more than one code location");
                    }
                }
                SynIoOption::Indent(indent) => {
                    if self.indent_opt.is_none() {
                        self.indent_opt = Some(indent);
                    } else {
                        return Err("more than one indentation");
                    }
                }
                SynIoOption::Headers(headers) => {
                    self.headers.extend(headers);
                }
            }
            Ok(())
        }
    }

    /// User-defined type for `io_option`
    #[derive(Clone, PartialEq, Debug)]
    pub enum SynIoOption {
        Error,
        Combined(Specification),
        Spec(Specification),
        Code(CodeLocation),
        Indent(usize),
        Headers(Vec<String>),
    }

    /// User-defined type for `<L> "," io_option` iteration in `io_options -> io_option ( ►► <L> "," io_option ◄◄ )*`
    pub type SynIIoOpt = SynIoOptions;

    /// User-defined type for `tag_opt`
    pub type SynTagOpt = Option<String>;

    /// User-defined type for `global_options`
    #[derive(Debug, PartialEq)]
    pub struct SynGlobalOptions {
        pub headers: Vec<String>,
        pub indent_opt: Option<usize>,
        pub libs: Vec<String>,
        pub nt_value_opt: Option<NTValue>,
        pub spans_opt: Option<bool>,
    }

    impl Default for SynGlobalOptions {
        fn default() -> Self {
            SynGlobalOptions {
                headers: vec![],
                indent_opt: None,
                libs: vec![],
                nt_value_opt: None,
                spans_opt: None,
            }
        }
    }

    impl SynGlobalOptions {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn fold(&mut self, global_option: SynGlobalOption) -> Result<(), &str> {
            match global_option {
                SynGlobalOption::Error => {}
                SynGlobalOption::Headers(headers) => {
                    self.headers.extend(headers);
                }
                SynGlobalOption::Indent(indent) => {
                    if self.indent_opt.is_none() {
                        self.indent_opt = Some(indent);
                    } else {
                        return Err("more than one indentation");
                    }
                }
                SynGlobalOption::Libs(libs) => {
                    self.libs.extend(libs);
                }
                SynGlobalOption::NTValue(nt_value) => {
                    if self.nt_value_opt.is_none() {
                        self.nt_value_opt = Some(nt_value);
                    } else if let Some(current) = self.nt_value_opt.as_mut() {
                        match (current, nt_value) {
                            // we allow to grow names
                            (NTValue::SetNames(names), NTValue::SetNames(new)) => {
                                names.extend(new);
                            }
                            _ => return Err("conflicting nt-value options"),
                        }
                    }
                }
                SynGlobalOption::Spans(spans) => {
                    if self.spans_opt.is_none() {
                        self.spans_opt = Some(spans);
                    } else {
                        return Err("more than one `spans` option")
                    }
                }
            }
            Ok(())
        }
    }

    /// User-defined type for `<L> "," global_option` iteration in `global_options -> global_option ( ►► <L> "," global_option ◄◄ )*`
    pub type SynIGlobalOpt = SynGlobalOptions;

    /// User-defined type for `global_option`
    #[derive(Debug, PartialEq)]
    pub enum SynGlobalOption {
        Error,
        Headers(Vec<String>),
        Indent(usize),
        Libs(Vec<String>),
        NTValue(NTValue),
        Spans(bool),
    }

    /// User-defined type for `value`
    #[derive(Clone, PartialEq, Debug)]
    pub enum SynValue {
        Error,
        Bool(bool),
        Num(usize),
        Str(String),
        CodeStdout,
    }

    /// User-defined type for `nt_value`
    pub type SynNtValue = NTValue;
}

#[allow(unused)]
mod config_lexer {
    // [config_lexer]

    use lexi_gram::lexigram_lib::lexigram_core;

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 39;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 12;
    const NBR_STATES: StateId = 113;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         37,  37,  37,  37,  37,  37,  37,  37,  37,  24,  38,  37,  37,  38,  37,  37,   // 0-15
         37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,   // 16-31
          0,  37,   1,  37,  37,  37,  37,  37,  37,  37,  25,  37,   2,  33,  37,   3,   // 32-47
          4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   5,   6,  37,   7,  37,  37,   // 48-63
         37,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,   // 64-79
         32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,   9,  26,  10,  37,  27,   // 80-95
         37,  28,  35,  11,  12,  30,  13,  32,  14,  15,  32,  32,  16,  34,  17,  18,   // 96-111
         19,  32,  31,  20,  21,  29,   8,  32,  36,  32,  32,  22,  37,  23,  37,  37,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 37),
        (Seg(57344, 1114111), 37),
    ];
    static TERMINAL_TABLE: [Terminal;101] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 4408] = [
         12,   1,  13,   2,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  12, 113, 113, 113,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113,  12, // state 0
          8, 113,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8, 113,   8,   9,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8, 113, // state 1
        113, 113, 113, 111, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,  10, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 2
        113, 113, 113, 113, 113, 113, 113, 113,   4, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 3
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,   5, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 4
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,   6, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 5
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,   7, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 6
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,  72, 113, 113, 113, 113, 113, 113, 113, 113, // state 7
          8, 110,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8, 113,   8,   9,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8, 113, // state 8
        113,   8, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,   8, 113, 113, 113,   8, 113, 113, 113, 113,   8, 113, 113, 113, 113,   8, 113, 113, 113, 113, 113, 113, 113, // state 9
         10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  11,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10, // state 10
         10,  10,  10, 112,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  11,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,  10, // state 11
         12, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,  12, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,  12, // state 12 <skip>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 13 <end:1>
        113, 113, 113, 113,  14, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 14 <end:27>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 15 <end:0>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 16 <end:7>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 17 <end:2>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 18 <end:26>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 19 <end:4>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 20 <end:6>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  34,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 21 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  41,  18,  18, 113,  18,  18,  18, 113, 113, // state 22 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18, 104,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 23 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  47,  18,  18, 113,  18,  18,  18, 113, 113, // state 24 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  53,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 25 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  62,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  61,  18,  18, 113,  18,  18,  18, 113, 113, // state 26 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  68,  18,  18,  69, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 27 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  73,  18,  18, 113, 113, 113, 113, 113,  18,  18,  74,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 28 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  84,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 29 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  94,  18,  95, 113, 113, 113, 113, 113,  18,  18,  18,  93,  18,  18, 113,  18,  18,  18, 113, 113, // state 30 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18, 108,  18, 113,  18,  18,  18, 113, 113, // state 31 <end:26>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 32 <end:3>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 33 <end:5>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  35,  18,  18, 113, 113, // state 34 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  36,  18, 113, 113, // state 35 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  37,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 36 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  38,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 37 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  39,  18,  18, 113,  18,  18,  18, 113, 113, // state 38 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  40,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 39 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 40 <end:8>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  42,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 41 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  43,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 42 <end:9>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  44,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 43 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  45,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 44 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  46, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 45 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 46 <end:10>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  48,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 47 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  49,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 48 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  50,  18,  18, 113,  18,  18,  18, 113, 113, // state 49 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  51,  18, 113,  18,  18,  18, 113, 113, // state 50 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  52,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 51 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 52 <end:11>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  54,  18,  18,  18,  18,  18,  18,  55,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 53 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  56,  18,  18, 113,  18,  18,  18, 113, 113, // state 54 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  59,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 55 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  57,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 56 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  58, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 57 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 58 <end:12>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  60, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 59 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 60 <end:13>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  63, 113, 113, // state 61 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  66,  18, 113, 113, // state 62 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  64,  18,  18, 113,  18,  18,  18, 113, 113, // state 63 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  65,  18, 113,  18,  18,  18, 113, 113, // state 64 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 65 <end:14>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  67,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 66 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 67 <end:15>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  70,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 68 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18,   3,  18,  18,  18, 113, 113, // state 69 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  71,  18,  18, 113,  18,  18,  18, 113, 113, // state 70 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 71 <end:16>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 72 <end:17>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  75, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 73 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  80, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 74 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  76,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 75 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  77,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 76 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  78,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 77 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  79,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 78 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 79 <end:18>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  81,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 80 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  82,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 81 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  83, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 82 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 83 <end:19>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  85,  18, 113,  18,  18,  18, 113, 113, // state 84 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  87,  18, 113, 113, 113, 113, 113,  18,  18,  18,  86,  18,  18, 113,  18,  18,  18, 113, 113, // state 85 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  88,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 86 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  91,  18,  18, 113,  18,  18,  18, 113, 113, // state 87 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  89, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 88 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  90,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 89 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 90 <end:20>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  92,  18, 113,  18,  18,  18, 113, 113, // state 91 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 92 <end:21>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  96, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 93 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  97,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 94 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18, 100,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 95 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 96 <end:22>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  98,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 97 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  99,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 98 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 99 <end:23>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18, 101,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 100 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18, 102,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 101 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 103, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 102 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 103 <end:24>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18, 105,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 104 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18, 106,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 105 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18, 107,  18,  18, 113,  18,  18,  18, 113, 113, // state 106 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 107 <end:25>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18, 109,  18,  18,  18, 113,  18,  18,  18, 113, 113, // state 108 <end:26>
        113, 113, 113, 113,  18, 113, 113, 113,  18, 113, 113,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18, 113, 113, 113, 113, 113,  18,  18,  18, 107,  18,  18, 113,  18,  18,  18, 113, 113, // state 109 <end:26>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 110 <end:28>
        111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 113, // state 111 <skip>
        113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, // state 112 <skip>
        113 // error group in [nbr_state * nbr_group + nbr_group]
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

    // [config_lexer]
}

#[allow(unused)]
mod config_parser {
    // [config_parser]

    use lexi_gram::lexigram_lib::lexigram_core;

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::{LogMsg, Logger}, parser::{Call, LLParser, ListenerWrapper, OpCode, Terminate}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 29;
    const PARSER_NUM_NT: usize = 19;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Colon", Some(":")), ("Comma", Some(",")), ("Equal", Some("=")), ("Lbracket", Some("{")), ("LSbracket", Some("[")), ("Rbracket", Some("}")), ("RSbracket", Some("]")), ("Semicolon", Some(";")), ("Combined", Some("combined")), ("Def", Some("def")), ("Default", Some("default")), ("Headers", Some("headers")), ("Indent", Some("indent")), ("Input", Some("input")), ("Lexer", Some("lexer")), ("Libs", Some("libs")), ("None", Some("none")), ("NTValue", Some("nt-value")), ("Options", Some("options")), ("Output", Some("output")), ("Parents", Some("parents")), ("Parser", Some("parser")), ("Set", Some("set")), ("Spans", Some("spans")), ("Stdout", Some("stdout")), ("BoolLiteral", None), ("Id", None), ("NumLiteral", None), ("StrLiteral", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["config", "definitions", "i_def", "lexer", "parser", "options", "io_options", "i_io_opt", "io_option", "tag_opt", "global_options", "i_global_opt", "global_option", "value", "nt_value", "io_option_1", "global_option_1", "global_option_2", "nt_value_1"];
    static ALT_VAR: [VarId; 44] = [0, 1, 2, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8, 8, 8, 8, 8, 9, 9, 10, 11, 11, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18];
    static PARSING_TABLE: [AltId; 570] = [44, 44, 44, 44, 44, 44, 44, 44, 44, 0, 44, 44, 44, 44, 0, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 45, 44, 44, 44, 44, 44, 44, 44, 44, 44, 1, 44, 44, 44, 44, 1, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 2, 44, 44, 44, 44, 3, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 4, 44, 44, 44, 45, 44, 44, 45, 44, 44, 44, 44, 44, 44, 44, 45, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 6, 44, 44, 5, 44, 44, 44, 44, 44, 44, 44, 6, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 7, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 8, 44, 44, 44, 44, 44, 45, 44, 44, 9, 44, 44, 9, 9, 9, 44, 44, 44, 44, 44, 9, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 10, 44, 44, 44, 11, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 45, 44, 44, 44, 45, 44, 44, 12, 44, 44, 16, 15, 13, 44, 44, 44, 44, 44, 14, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 18, 44, 44, 17, 18, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 45, 44, 44, 44, 44, 44, 19, 19, 44, 44, 19, 44, 19, 44, 44, 44, 44, 44, 19, 44, 44, 44, 44, 44, 44, 44, 20, 44, 44, 44, 21, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 45, 44, 44, 44, 45, 44, 44, 44, 44, 44, 22, 23, 44, 44, 24, 44, 25, 44, 44, 44, 44, 44, 26, 44, 44, 44, 44, 44, 44, 44, 45, 44, 44, 45, 45, 45, 45, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 31, 27, 30, 28, 29, 44, 44, 45, 44, 44, 44, 45, 44, 44, 44, 44, 32, 44, 44, 44, 44, 44, 33, 44, 44, 44, 34, 44, 35, 44, 44, 44, 44, 44, 44, 44, 44, 36, 44, 44, 44, 37, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 38, 44, 44, 44, 39, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 40, 44, 44, 44, 41, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 42, 44, 44, 44, 43, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44];
    static OPCODES: [&[OpCode]; 44] = [&[OpCode::Exit(0), OpCode::NT(5), OpCode::NT(4), OpCode::NT(3), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Loop(2), OpCode::Exit(2), OpCode::T(7), OpCode::NT(13), OpCode::T(2), OpCode::T(26), OpCode::T(9)], &[OpCode::Exit(3)], &[OpCode::Exit(4), OpCode::T(5), OpCode::NT(6), OpCode::T(3), OpCode::T(14)], &[OpCode::Exit(5), OpCode::T(5), OpCode::NT(6), OpCode::T(3), OpCode::T(21)], &[OpCode::Exit(6)], &[OpCode::Exit(7), OpCode::T(5), OpCode::NT(10), OpCode::T(3), OpCode::T(18)], &[OpCode::Exit(8)], &[OpCode::Exit(9), OpCode::NT(7), OpCode::NT(8)], &[OpCode::Loop(7), OpCode::Exit(10), OpCode::NT(8), OpCode::T(1)], &[OpCode::Exit(11)], &[OpCode::Exit(12), OpCode::NT(9), OpCode::NT(13), OpCode::T(0), OpCode::T(8)], &[OpCode::Exit(13), OpCode::NT(9), OpCode::NT(13), OpCode::T(0), OpCode::T(13)], &[OpCode::Exit(14), OpCode::NT(9), OpCode::NT(13), OpCode::T(0), OpCode::T(19)], &[OpCode::Exit(15), OpCode::NT(13), OpCode::T(0), OpCode::T(12)], &[OpCode::Exit(16), OpCode::T(5), OpCode::NT(15), OpCode::NT(13), OpCode::T(3), OpCode::T(0), OpCode::T(11)], &[OpCode::Exit(17), OpCode::T(6), OpCode::NT(13), OpCode::T(4)], &[OpCode::Exit(18)], &[OpCode::Exit(19), OpCode::NT(11), OpCode::NT(12)], &[OpCode::Loop(11), OpCode::Exit(20), OpCode::NT(12), OpCode::T(1)], &[OpCode::Exit(21)], &[OpCode::Exit(22), OpCode::T(5), OpCode::NT(16), OpCode::NT(13), OpCode::T(3), OpCode::T(0), OpCode::T(11)], &[OpCode::Exit(23), OpCode::NT(13), OpCode::T(0), OpCode::T(12)], &[OpCode::Exit(24), OpCode::T(5), OpCode::NT(17), OpCode::NT(13), OpCode::T(3), OpCode::T(0), OpCode::T(15)], &[OpCode::Exit(25), OpCode::NT(14), OpCode::T(0), OpCode::T(17)], &[OpCode::Exit(26), OpCode::NT(13), OpCode::T(0), OpCode::T(23)], &[OpCode::Exit(27), OpCode::T(25)], &[OpCode::Exit(28), OpCode::T(27)], &[OpCode::Exit(29), OpCode::T(28)], &[OpCode::Exit(30), OpCode::T(26)], &[OpCode::Exit(31), OpCode::T(24)], &[OpCode::Exit(32), OpCode::T(10)], &[OpCode::Exit(33), OpCode::T(16)], &[OpCode::Exit(34), OpCode::T(20)], &[OpCode::Exit(35), OpCode::T(5), OpCode::NT(18), OpCode::NT(13), OpCode::T(3), OpCode::T(22)], &[OpCode::Loop(15), OpCode::Exit(36), OpCode::NT(13), OpCode::T(1)], &[OpCode::Exit(37)], &[OpCode::Loop(16), OpCode::Exit(38), OpCode::NT(13), OpCode::T(1)], &[OpCode::Exit(39)], &[OpCode::Loop(17), OpCode::Exit(40), OpCode::NT(13), OpCode::T(1)], &[OpCode::Exit(41)], &[OpCode::Loop(18), OpCode::Exit(42), OpCode::NT(13), OpCode::T(1)], &[OpCode::Exit(43)]];
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
    pub enum CtxConfig {
        /// `config -> definitions lexer parser options`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxDefinitions {
        /// `definitions -> (<L> "def" Id "=" value ";")*`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxIDef {
        /// `<L> "def" Id "=" value ";"` iteration in `definitions -> ( ►► <L> "def" Id "=" value ";" ◄◄ )*`
        V1 { id: String, value: SynValue },
    }
    #[derive(Debug)]
    pub enum CtxLexer {
        /// `lexer -> "lexer" "{" io_options "}"`
        V1 { io_options: SynIoOptions },
    }
    #[derive(Debug)]
    pub enum CtxParser {
        /// `parser -> "parser" "{" io_options "}"`
        V1 { io_options: SynIoOptions },
        /// `parser -> ε`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxOptions {
        /// `options -> "options" "{" global_options "}"`
        V1 { global_options: SynGlobalOptions },
        /// `options -> ε`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxIoOptions {
        /// `io_options -> io_option (<L> "," io_option)*`
        V1 { star: SynIIoOpt },
    }
    #[derive(Debug)]
    pub enum InitCtxIIoOpt {
        /// value of `io_option` before `<L> "," io_option` iteration in `io_options -> io_option ( ►► <L> "," io_option ◄◄ )*`
        V1 { io_option: SynIoOption },
    }
    #[derive(Debug)]
    pub enum CtxIIoOpt {
        /// `<L> "," io_option` iteration in `io_options -> io_option ( ►► <L> "," io_option ◄◄ )*`
        V1 { io_option: SynIoOption },
    }
    #[derive(Debug)]
    pub enum CtxIoOption {
        /// `io_option -> "combined" ":" value tag_opt`
        V1 { value: SynValue, tag_opt: SynTagOpt },
        /// `io_option -> "input" ":" value tag_opt`
        V2 { value: SynValue, tag_opt: SynTagOpt },
        /// `io_option -> "output" ":" value tag_opt`
        V3 { value: SynValue, tag_opt: SynTagOpt },
        /// `io_option -> "indent" ":" value`
        V4 { value: SynValue },
        /// `io_option -> "headers" ":" "{" value ("," value)* "}"`
        V5 { star: SynIoOption1 },
    }
    #[derive(Debug)]
    pub enum CtxTagOpt {
        /// `tag_opt -> "[" value "]"`
        V1 { value: SynValue },
        /// `tag_opt -> ε`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxGlobalOptions {
        /// `global_options -> global_option (<L> "," global_option)*`
        V1 { star: SynIGlobalOpt },
    }
    #[derive(Debug)]
    pub enum InitCtxIGlobalOpt {
        /// value of `global_option` before `<L> "," global_option` iteration in `global_options -> global_option ( ►► <L> "," global_option ◄◄ )*`
        V1 { global_option: SynGlobalOption },
    }
    #[derive(Debug)]
    pub enum CtxIGlobalOpt {
        /// `<L> "," global_option` iteration in `global_options -> global_option ( ►► <L> "," global_option ◄◄ )*`
        V1 { global_option: SynGlobalOption },
    }
    #[derive(Debug)]
    pub enum CtxGlobalOption {
        /// `global_option -> "headers" ":" "{" value ("," value)* "}"`
        V1 { star: SynGlobalOption1 },
        /// `global_option -> "indent" ":" value`
        V2 { value: SynValue },
        /// `global_option -> "libs" ":" "{" value ("," value)* "}"`
        V3 { star: SynGlobalOption2 },
        /// `global_option -> "nt-value" ":" nt_value`
        V4 { nt_value: SynNtValue },
        /// `global_option -> "spans" ":" value`
        V5 { value: SynValue },
    }
    #[derive(Debug)]
    pub enum CtxValue {
        /// `value -> BoolLiteral`
        V1 { boolliteral: String },
        /// `value -> NumLiteral`
        V2 { numliteral: String },
        /// `value -> StrLiteral`
        V3 { strliteral: String },
        /// `value -> Id`
        V4 { id: String },
        /// `value -> "stdout"`
        V5,
    }
    #[derive(Debug)]
    pub enum CtxNtValue {
        /// `nt_value -> "default"`
        V1,
        /// `nt_value -> "none"`
        V2,
        /// `nt_value -> "parents"`
        V3,
        /// `nt_value -> "set" "{" value ("," value)* "}"`
        V4 { star: SynNtValue1 },
    }

    /// Computed `("," value)*` array in `io_option -> "combined" ":" value tag_opt | "input" ":" value tag_opt | "output" ":" value tag_opt | "indent" ":" value | "headers" ":" "{" value  ►► ("," value)* ◄◄  "}"`
    #[derive(Debug, PartialEq)]
    pub struct SynIoOption1(pub Vec<SynValue>);
    /// Computed `("," value)*` array in `global_option -> "headers" ":" "{" value  ►► ("," value)* ◄◄  "}" | "indent" ":" value | "libs" ":" "{" value ("," value)* "}" | "nt-value" ":" nt_value | "spans" ":" value`
    #[derive(Debug, PartialEq)]
    pub struct SynGlobalOption1(pub Vec<SynValue>);
    /// Computed `("," value)*` array in `global_option -> "headers" ":" "{" value ("," value)* "}" | "indent" ":" value | "libs" ":" "{" value  ►► ("," value)* ◄◄  "}" | "nt-value" ":" nt_value | "spans" ":" value`
    #[derive(Debug, PartialEq)]
    pub struct SynGlobalOption2(pub Vec<SynValue>);
    /// Computed `("," value)*` array in `nt_value -> "default" | "none" | "parents" | "set" "{" value  ►► ("," value)* ◄◄  "}"`
    #[derive(Debug, PartialEq)]
    pub struct SynNtValue1(pub Vec<SynValue>);
    /// Top non-terminal Config (has no value)
    #[derive(Debug, PartialEq)]
    pub struct SynConfig();

    #[derive(Debug)]
    enum EnumSynValue { IoOptions(SynIoOptions), IIoOpt(SynIIoOpt), IoOption(SynIoOption), TagOpt(SynTagOpt), GlobalOptions(SynGlobalOptions), IGlobalOpt(SynIGlobalOpt), GlobalOption(SynGlobalOption), Value(SynValue), NtValue(SynNtValue), IoOption1(SynIoOption1), GlobalOption1(SynGlobalOption1), GlobalOption2(SynGlobalOption2), NtValue1(SynNtValue1) }

    impl EnumSynValue {
        fn get_io_options(self) -> SynIoOptions {
            if let EnumSynValue::IoOptions(val) = self { val } else { panic!() }
        }
        fn get_i_io_opt(self) -> SynIIoOpt {
            if let EnumSynValue::IIoOpt(val) = self { val } else { panic!() }
        }
        fn get_io_option(self) -> SynIoOption {
            if let EnumSynValue::IoOption(val) = self { val } else { panic!() }
        }
        fn get_tag_opt(self) -> SynTagOpt {
            if let EnumSynValue::TagOpt(val) = self { val } else { panic!() }
        }
        fn get_global_options(self) -> SynGlobalOptions {
            if let EnumSynValue::GlobalOptions(val) = self { val } else { panic!() }
        }
        fn get_i_global_opt(self) -> SynIGlobalOpt {
            if let EnumSynValue::IGlobalOpt(val) = self { val } else { panic!() }
        }
        fn get_global_option(self) -> SynGlobalOption {
            if let EnumSynValue::GlobalOption(val) = self { val } else { panic!() }
        }
        fn get_value(self) -> SynValue {
            if let EnumSynValue::Value(val) = self { val } else { panic!() }
        }
        fn get_nt_value(self) -> SynNtValue {
            if let EnumSynValue::NtValue(val) = self { val } else { panic!() }
        }
        fn get_io_option1(self) -> SynIoOption1 {
            if let EnumSynValue::IoOption1(val) = self { val } else { panic!() }
        }
        fn get_global_option1(self) -> SynGlobalOption1 {
            if let EnumSynValue::GlobalOption1(val) = self { val } else { panic!() }
        }
        fn get_global_option2(self) -> SynGlobalOption2 {
            if let EnumSynValue::GlobalOption2(val) = self { val } else { panic!() }
        }
        fn get_nt_value1(self) -> SynNtValue1 {
            if let EnumSynValue::NtValue1(val) = self { val } else { panic!() }
        }
    }

    pub trait ConfigListener {
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
        fn exit(&mut self, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_config(&mut self) {}
        #[allow(unused_variables)]
        fn exit_config(&mut self, ctx: CtxConfig, spans: Vec<PosSpan>) {}
        fn init_definitions(&mut self) {}
        #[allow(unused_variables)]
        fn exit_definitions(&mut self, ctx: CtxDefinitions, spans: Vec<PosSpan>) {}
        fn init_i_def(&mut self) {}
        #[allow(unused_variables)]
        fn exit_i_def(&mut self, ctx: CtxIDef, spans: Vec<PosSpan>) {}
        fn init_lexer(&mut self) {}
        #[allow(unused_variables)]
        fn exit_lexer(&mut self, ctx: CtxLexer, spans: Vec<PosSpan>) {}
        fn init_parser(&mut self) {}
        #[allow(unused_variables)]
        fn exit_parser(&mut self, ctx: CtxParser, spans: Vec<PosSpan>) {}
        fn init_options(&mut self) {}
        #[allow(unused_variables)]
        fn exit_options(&mut self, ctx: CtxOptions, spans: Vec<PosSpan>) {}
        fn init_io_options(&mut self) {}
        fn exit_io_options(&mut self, ctx: CtxIoOptions, spans: Vec<PosSpan>) -> SynIoOptions;
        fn init_i_io_opt(&mut self, ctx: InitCtxIIoOpt, spans: Vec<PosSpan>) -> SynIIoOpt;
        fn exit_i_io_opt(&mut self, acc: &mut SynIIoOpt, ctx: CtxIIoOpt, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_i_io_opt(&mut self, acc: &mut SynIIoOpt) {}
        fn init_io_option(&mut self) {}
        fn exit_io_option(&mut self, ctx: CtxIoOption, spans: Vec<PosSpan>) -> SynIoOption;
        fn init_tag_opt(&mut self) {}
        fn exit_tag_opt(&mut self, ctx: CtxTagOpt, spans: Vec<PosSpan>) -> SynTagOpt;
        fn init_global_options(&mut self) {}
        fn exit_global_options(&mut self, ctx: CtxGlobalOptions, spans: Vec<PosSpan>) -> SynGlobalOptions;
        fn init_i_global_opt(&mut self, ctx: InitCtxIGlobalOpt, spans: Vec<PosSpan>) -> SynIGlobalOpt;
        fn exit_i_global_opt(&mut self, acc: &mut SynIGlobalOpt, ctx: CtxIGlobalOpt, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_i_global_opt(&mut self, acc: &mut SynIGlobalOpt) {}
        fn init_global_option(&mut self) {}
        fn exit_global_option(&mut self, ctx: CtxGlobalOption, spans: Vec<PosSpan>) -> SynGlobalOption;
        fn init_value(&mut self) {}
        fn exit_value(&mut self, ctx: CtxValue, spans: Vec<PosSpan>) -> SynValue;
        fn init_nt_value(&mut self) {}
        fn exit_nt_value(&mut self, ctx: CtxNtValue, spans: Vec<PosSpan>) -> SynNtValue;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<EnumSynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
        stack_span: Vec<PosSpan>,
    }

    impl<T: ConfigListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    if matches!(nt, 2) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_config(),           // config
                        1 => self.listener.init_definitions(),      // definitions
                        2 => self.listener.init_i_def(),            // i_def
                        3 => self.listener.init_lexer(),            // lexer
                        4 => self.listener.init_parser(),           // parser
                        5 => self.listener.init_options(),          // options
                        6 => self.listener.init_io_options(),       // io_options
                        7 => self.init_i_io_opt(),                  // i_io_opt
                        8 => self.listener.init_io_option(),        // io_option
                        15 => self.init_io_option1(),               // io_option_1
                        9 => self.listener.init_tag_opt(),          // tag_opt
                        10 => self.listener.init_global_options(),  // global_options
                        11 => self.init_i_global_opt(),             // i_global_opt
                        12 => self.listener.init_global_option(),   // global_option
                        16 => self.init_global_option1(),           // global_option_1
                        17 => self.init_global_option2(),           // global_option_2
                        13 => self.listener.init_value(),           // value
                        14 => self.listener.init_nt_value(),        // nt_value
                        18 => self.init_nt_value1(),                // nt_value_1
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_config(),                    // config -> definitions lexer parser options
                        1 => self.exit_definitions(),               // definitions -> i_def
                        2 => self.exit_i_def(),                     // i_def -> <L> "def" Id "=" value ";" i_def
                        3 => {}                                     // i_def -> <L> ε (not used)
                        4 => self.exit_lexer(),                     // lexer -> "lexer" "{" io_options "}"
                        5 |                                         // parser -> "parser" "{" io_options "}"
                        6 => self.exit_parser(alt_id),              // parser -> ε
                        7 |                                         // options -> "options" "{" global_options "}"
                        8 => self.exit_options(alt_id),             // options -> ε
                        9 => self.exit_io_options(),                // io_options -> io_option i_io_opt
                        10 => self.exit_i_io_opt(),                 // i_io_opt -> <L> "," io_option i_io_opt
                        11 => self.exitloop_i_io_opt(),             // i_io_opt -> <L> ε
                        12 |                                        // io_option -> "combined" ":" value tag_opt
                        13 |                                        // io_option -> "input" ":" value tag_opt
                        14 |                                        // io_option -> "output" ":" value tag_opt
                        15 |                                        // io_option -> "indent" ":" value
                        16 => self.exit_io_option(alt_id),          // io_option -> "headers" ":" "{" value io_option_1 "}"
                        36 => self.exit_io_option1(),               // io_option_1 -> "," value io_option_1
                        37 => {}                                    // io_option_1 -> ε
                        17 |                                        // tag_opt -> "[" value "]"
                        18 => self.exit_tag_opt(alt_id),            // tag_opt -> ε
                        19 => self.exit_global_options(),           // global_options -> global_option i_global_opt
                        20 => self.exit_i_global_opt(),             // i_global_opt -> <L> "," global_option i_global_opt
                        21 => self.exitloop_i_global_opt(),         // i_global_opt -> <L> ε
                        22 |                                        // global_option -> "headers" ":" "{" value global_option_1 "}"
                        23 |                                        // global_option -> "indent" ":" value
                        24 |                                        // global_option -> "libs" ":" "{" value global_option_2 "}"
                        25 |                                        // global_option -> "nt-value" ":" nt_value
                        26 => self.exit_global_option(alt_id),      // global_option -> "spans" ":" value
                        38 => self.exit_global_option1(),           // global_option_1 -> "," value global_option_1
                        39 => {}                                    // global_option_1 -> ε
                        40 => self.exit_global_option2(),           // global_option_2 -> "," value global_option_2
                        41 => {}                                    // global_option_2 -> ε
                        27 |                                        // value -> BoolLiteral
                        28 |                                        // value -> NumLiteral
                        29 |                                        // value -> StrLiteral
                        30 |                                        // value -> Id
                        31 => self.exit_value(alt_id),              // value -> "stdout"
                        32 |                                        // nt_value -> "default"
                        33 |                                        // nt_value -> "none"
                        34 |                                        // nt_value -> "parents"
                        35 => self.exit_nt_value(alt_id),           // nt_value -> "set" "{" value nt_value_1 "}"
                        42 => self.exit_nt_value1(),                // nt_value_1 -> "," value nt_value_1
                        43 => {}                                    // nt_value_1 -> ε
                        _ => panic!("unexpected exit alternative id: {alt_id}")
                    }
                }
                Call::End(terminate) => {
                    match terminate {
                        Terminate::None => {
                            let span = self.stack_span.pop().unwrap();
                            self.listener.exit(span);
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

    impl<T: ConfigListener> Wrapper<T> {
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

        fn exit_config(&mut self) {
            let ctx = CtxConfig::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_config(ctx, spans);
        }

        fn exit_definitions(&mut self) {
            let ctx = CtxDefinitions::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_definitions(ctx, spans);
        }

        fn exit_i_def(&mut self) {
            let value = self.stack.pop().unwrap().get_value();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxIDef::V1 { id, value };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_i_def(ctx, spans);
        }

        fn exit_lexer(&mut self) {
            let io_options = self.stack.pop().unwrap().get_io_options();
            let ctx = CtxLexer::V1 { io_options };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_lexer(ctx, spans);
        }

        fn exit_parser(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                5 => {
                    let io_options = self.stack.pop().unwrap().get_io_options();
                    (4, CtxParser::V1 { io_options })
                }
                6 => {
                    (0, CtxParser::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_parser")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_parser(ctx, spans);
        }

        fn exit_options(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                7 => {
                    let global_options = self.stack.pop().unwrap().get_global_options();
                    (4, CtxOptions::V1 { global_options })
                }
                8 => {
                    (0, CtxOptions::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_options")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_options(ctx, spans);
        }

        fn exit_io_options(&mut self) {
            let star = self.stack.pop().unwrap().get_i_io_opt();
            let ctx = CtxIoOptions::V1 { star };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_io_options(ctx, spans);
            self.stack.push(EnumSynValue::IoOptions(val));
        }

        fn init_i_io_opt(&mut self) {
            let io_option = self.stack.pop().unwrap().get_io_option();
            let ctx = InitCtxIIoOpt::V1 { io_option };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.init_i_io_opt(ctx, spans);
            self.stack.push(EnumSynValue::IIoOpt(val));
        }

        fn exit_i_io_opt(&mut self) {
            let io_option = self.stack.pop().unwrap().get_io_option();
            let ctx = CtxIIoOpt::V1 { io_option };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::IIoOpt(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_i_io_opt(acc, ctx, spans);
        }

        fn exitloop_i_io_opt(&mut self) {
            let EnumSynValue::IIoOpt(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_i_io_opt(acc);
        }

        fn exit_io_option(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                12 => {
                    let tag_opt = self.stack.pop().unwrap().get_tag_opt();
                    let value = self.stack.pop().unwrap().get_value();
                    (4, CtxIoOption::V1 { value, tag_opt })
                }
                13 => {
                    let tag_opt = self.stack.pop().unwrap().get_tag_opt();
                    let value = self.stack.pop().unwrap().get_value();
                    (4, CtxIoOption::V2 { value, tag_opt })
                }
                14 => {
                    let tag_opt = self.stack.pop().unwrap().get_tag_opt();
                    let value = self.stack.pop().unwrap().get_value();
                    (4, CtxIoOption::V3 { value, tag_opt })
                }
                15 => {
                    let value = self.stack.pop().unwrap().get_value();
                    (3, CtxIoOption::V4 { value })
                }
                16 => {
                    let star = self.stack.pop().unwrap().get_io_option1();
                    (5, CtxIoOption::V5 { star })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_io_option")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_io_option(ctx, spans);
            self.stack.push(EnumSynValue::IoOption(val));
        }

        fn init_io_option1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let value = self.stack.pop().unwrap().get_value();
            self.stack.push(EnumSynValue::IoOption1(SynIoOption1(vec![value])));
        }

        fn exit_io_option1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let value = self.stack.pop().unwrap().get_value();
            let Some(EnumSynValue::IoOption1(SynIoOption1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynIoOption1 item on wrapper stack");
            };
            star_acc.push(value);
        }

        fn exit_tag_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                17 => {
                    let value = self.stack.pop().unwrap().get_value();
                    (3, CtxTagOpt::V1 { value })
                }
                18 => {
                    (0, CtxTagOpt::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_tag_opt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_tag_opt(ctx, spans);
            self.stack.push(EnumSynValue::TagOpt(val));
        }

        fn exit_global_options(&mut self) {
            let star = self.stack.pop().unwrap().get_i_global_opt();
            let ctx = CtxGlobalOptions::V1 { star };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_global_options(ctx, spans);
            self.stack.push(EnumSynValue::GlobalOptions(val));
        }

        fn init_i_global_opt(&mut self) {
            let global_option = self.stack.pop().unwrap().get_global_option();
            let ctx = InitCtxIGlobalOpt::V1 { global_option };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.init_i_global_opt(ctx, spans);
            self.stack.push(EnumSynValue::IGlobalOpt(val));
        }

        fn exit_i_global_opt(&mut self) {
            let global_option = self.stack.pop().unwrap().get_global_option();
            let ctx = CtxIGlobalOpt::V1 { global_option };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::IGlobalOpt(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_i_global_opt(acc, ctx, spans);
        }

        fn exitloop_i_global_opt(&mut self) {
            let EnumSynValue::IGlobalOpt(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_i_global_opt(acc);
        }

        fn exit_global_option(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                22 => {
                    let star = self.stack.pop().unwrap().get_global_option1();
                    (5, CtxGlobalOption::V1 { star })
                }
                23 => {
                    let value = self.stack.pop().unwrap().get_value();
                    (3, CtxGlobalOption::V2 { value })
                }
                24 => {
                    let star = self.stack.pop().unwrap().get_global_option2();
                    (5, CtxGlobalOption::V3 { star })
                }
                25 => {
                    let nt_value = self.stack.pop().unwrap().get_nt_value();
                    (3, CtxGlobalOption::V4 { nt_value })
                }
                26 => {
                    let value = self.stack.pop().unwrap().get_value();
                    (3, CtxGlobalOption::V5 { value })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_global_option")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_global_option(ctx, spans);
            self.stack.push(EnumSynValue::GlobalOption(val));
        }

        fn init_global_option1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let value = self.stack.pop().unwrap().get_value();
            self.stack.push(EnumSynValue::GlobalOption1(SynGlobalOption1(vec![value])));
        }

        fn exit_global_option1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let value = self.stack.pop().unwrap().get_value();
            let Some(EnumSynValue::GlobalOption1(SynGlobalOption1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynGlobalOption1 item on wrapper stack");
            };
            star_acc.push(value);
        }

        fn init_global_option2(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let value = self.stack.pop().unwrap().get_value();
            self.stack.push(EnumSynValue::GlobalOption2(SynGlobalOption2(vec![value])));
        }

        fn exit_global_option2(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let value = self.stack.pop().unwrap().get_value();
            let Some(EnumSynValue::GlobalOption2(SynGlobalOption2(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynGlobalOption2 item on wrapper stack");
            };
            star_acc.push(value);
        }

        fn exit_value(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                27 => {
                    let boolliteral = self.stack_t.pop().unwrap();
                    (1, CtxValue::V1 { boolliteral })
                }
                28 => {
                    let numliteral = self.stack_t.pop().unwrap();
                    (1, CtxValue::V2 { numliteral })
                }
                29 => {
                    let strliteral = self.stack_t.pop().unwrap();
                    (1, CtxValue::V3 { strliteral })
                }
                30 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxValue::V4 { id })
                }
                31 => {
                    (1, CtxValue::V5)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_value")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_value(ctx, spans);
            self.stack.push(EnumSynValue::Value(val));
        }

        fn exit_nt_value(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                32 => {
                    (1, CtxNtValue::V1)
                }
                33 => {
                    (1, CtxNtValue::V2)
                }
                34 => {
                    (1, CtxNtValue::V3)
                }
                35 => {
                    let star = self.stack.pop().unwrap().get_nt_value1();
                    (4, CtxNtValue::V4 { star })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_nt_value")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nt_value(ctx, spans);
            self.stack.push(EnumSynValue::NtValue(val));
        }

        fn init_nt_value1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let value = self.stack.pop().unwrap().get_value();
            self.stack.push(EnumSynValue::NtValue1(SynNtValue1(vec![value])));
        }

        fn exit_nt_value1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let value = self.stack.pop().unwrap().get_value();
            let Some(EnumSynValue::NtValue1(SynNtValue1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynNtValue1 item on wrapper stack");
            };
            star_acc.push(value);
        }
    }

    // [config_parser]
}
