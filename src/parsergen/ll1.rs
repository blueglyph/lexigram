use lexigram_core::alt::Alternative;
use lexigram_core::parser::{LLParser, OpCode};
use lexigram_core::{AltId, VarId};
use lexigram_core::fixed_sym_table::FixedSymTable;
use lexigram_core::log::{LogReader, LogStatus};
use crate::build::{BuildError, BuildErrorSource, BuildFrom, TryBuildFrom};
use crate::grammar::ll1::LL1ParsingTable;
use crate::parsergen::ParserGen;

/// Tables and parameters used to create a [`LLParser`]. This type is used as a return object from the parser generator,
/// when the LLParser must be created dynamically; for example, in tests or in situations where the grammar isn't
/// known in advance. In those situations, the LLParserTables object must live as long as the parser it generates.
///
/// The LLParser itself uses references to tables whenever possible because, in most situations, the tables are
/// static in generated source files. A few fields must still be created dynamically from (possibly) static
/// tables because they don't exist in static form.
pub struct LLParserTables {
    num_nt: usize,
    num_t_full: usize,
    alt_var: Vec<VarId>,
    alts: Vec<Alternative>,
    opcodes: Vec<Vec<OpCode>>,
    init_opcodes: Vec<OpCode>,
    table: Vec<AltId>,
    symbol_table: FixedSymTable,
    start: VarId,
    include_alts: bool,
}

impl LLParserTables {
    pub fn new(
        parsing_table: LL1ParsingTable,
        symbol_table: FixedSymTable,
        opcodes: Vec<Vec<OpCode>>,
        init_opcodes: Vec<OpCode>,
        start: VarId,
        include_alts: bool
    ) -> Self {
        assert!(parsing_table.num_nt > start as usize);
        let num_nt = parsing_table.num_nt;
        let num_t_full = parsing_table.num_t_full;
        let table = parsing_table.table;
        let (factor_var, alts): (Vec<_>, Vec<_>) = parsing_table.alts.into_iter().unzip();
        LLParserTables { num_nt, num_t_full, alt_var: factor_var, alts, opcodes, init_opcodes, table, symbol_table, start, include_alts }
    }

    pub fn get_symbol_table(&self) -> &FixedSymTable {
        &self.symbol_table
    }
    
    pub fn make_parser(&self) -> LLParser<'_> {
        LLParser::new(
            self.num_nt,
            self.num_t_full,
            self.alt_var.as_slice(),
            if self.include_alts { self.alts.clone() } else { vec![] },
            self.opcodes.clone(),
            self.init_opcodes.clone(),
            self.table.as_slice(),
            self.symbol_table.clone(),
            self.start,
        )
    }
}

impl BuildFrom<ParserGen> for LLParserTables {
    /// Creates a [`LLParserTables`], from which a parser can be created dynamically with
    /// [`parser_table.make_parser()`](LLParserTables::make_parser).
    fn build_from(parser_gen: ParserGen) -> Self {
        if !parser_gen.has_no_errors() {
            panic!("creation of LL parser tables failed:{}", parser_gen.log);
        }
        let parsing_table = LL1ParsingTable {
            num_nt: parser_gen.num_nt,
            num_t_full: parser_gen.num_t_full,
            alts: parser_gen.alts,
            table: parser_gen.table,
            flags: parser_gen.flags,
            parent: parser_gen.parent,
        };
        LLParserTables::new(
            parsing_table,
            parser_gen.symbol_table.to_fixed_sym_table(),
            parser_gen.opcodes,
            parser_gen.init_opcodes,
            parser_gen.start,
            parser_gen.options.include_alts
        )
    }
}

// not generated automatically since LLParserTables isn't LogReader
impl TryBuildFrom<ParserGen> for LLParserTables {
    type Error = BuildError;

    fn try_build_from(source: ParserGen) -> Result<Self, Self::Error> {
        if source.get_log().has_no_errors() {
            Ok(LLParserTables::build_from(source))
        } else {
            Err(BuildError::new(source.give_log(), BuildErrorSource::ParserGen))
        }
    }
}
