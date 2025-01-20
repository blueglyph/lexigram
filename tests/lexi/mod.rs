use crate::gen::lexiparser::lexiparser::*;
use crate::gen::lexiparser::lexiparser_types::*;

struct LexiListener {
    verbose: bool
}

impl LexiListener {
    fn new() -> Self {
        LexiListener {
            verbose: false
        }
    }
}

impl LexiParserListener for LexiListener {
    fn exit(&mut self, _file: SynFile) {
        if self.verbose { println!("exit"); }
    }

    fn init_file(&mut self) {
        if self.verbose { println!("init_file"); }
    }

    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        if self.verbose { println!("exit_file"); }
        SynFile()
    }

    fn init_file_item(&mut self) {
        if self.verbose { println!("init_file_item"); }
    }

    fn exit_file_item(&mut self, _ctx: CtxFileItem) -> SynFileItem {
        if self.verbose { println!("exit_file_item"); }
        SynFileItem()
    }

    fn init_header(&mut self) {
        if self.verbose { println!("init_header"); }
    }

    fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader {
        if self.verbose { println!("exit_header"); }
        SynHeader()
    }

    fn init_declaration(&mut self) {
        if self.verbose { println!("init_declaration"); }
    }

    fn exit_declaration(&mut self, _ctx: CtxDeclaration) -> SynDeclaration {
        if self.verbose { println!("exit_declaration"); }
        SynDeclaration()
    }

    fn init_option(&mut self) {
        if self.verbose { println!("init_option"); }
    }

    fn exit_option(&mut self, _ctx: CtxOption) -> SynOption {
        if self.verbose { println!("exit_option"); }
        SynOption()
    }

    fn init_rule(&mut self) {
        if self.verbose { println!("init_rule"); }
    }

    fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule {
        if self.verbose { println!("exit_rule"); }
        SynRule()
    }

    fn init_actions(&mut self) {
        if self.verbose { println!("init_actions"); }
    }

    fn exit_actions(&mut self, _ctx: CtxActions) -> SynActions {
        if self.verbose { println!("exit_actions"); }
        SynActions()
    }

    fn init_action(&mut self) {
        if self.verbose { println!("init_action"); }
    }

    fn exit_action(&mut self, _ctx: CtxAction) -> SynAction {
        if self.verbose { println!("exit_action"); }
        SynAction()
    }

    fn init_match1(&mut self) {
        if self.verbose { println!("init_match1"); }
    }

    fn exit_match1(&mut self, _ctx: CtxMatch) -> SynMatch {
        if self.verbose { println!("exit_match1"); }
        SynMatch()
    }

    fn init_alt_items(&mut self) {
        if self.verbose { println!("init_alt_items"); }
    }

    fn exit_alt_items(&mut self, _ctx: CtxAltItems) -> SynAltItems {
        if self.verbose { println!("exit_alt_items"); }
        SynAltItems()
    }

    fn init_alt_item(&mut self) {
        if self.verbose { println!("init_alt_item"); }
    }

    fn exit_alt_item(&mut self, _ctx: CtxAltItem) -> SynAltItem {
        if self.verbose { println!("exit_alt_item"); }
        SynAltItem()
    }

    fn init_repeat_item(&mut self) {
        if self.verbose { println!("init_repeat_item"); }
    }

    fn exit_repeat_item(&mut self, _ctx: CtxRepeatItem) -> SynRepeatItem {
        if self.verbose { println!("exit_repeat_item"); }
        SynRepeatItem()
    }

    fn init_item(&mut self) {
        if self.verbose { println!("init_item"); }
    }

    fn exit_item(&mut self, _ctx: CtxItem) -> SynItem {
        if self.verbose { println!("exit_item"); }
        SynItem()
    }
    
    
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use rlexer::io::CharReader;
    use rlexer::lexi::LEXICON;
    use super::*;
    use crate::gen::build_lexer;
    use crate::gen::lexiparser::lexiparser::build_parser;

    #[test]
    fn lexer_parser() {
        let tests = vec![
            "",
            "lexicon LexiLexer;",
            LEXICON,
        ];
        const VERBOSE: bool = true;

        for (test_id, input) in tests.into_iter().enumerate() {
            if VERBOSE { println!("// {:=<80}\n// Test {test_id}", ""); }
            let stream = CharReader::new(Cursor::new(input));
            let mut lexer = build_lexer();
            lexer.set_tab_width(4);
            lexer.attach_stream(stream);

            let mut parser = build_parser();
            let mut listener = LexiListener::new();
            listener.verbose = VERBOSE;
            let mut wrapper = ListenerWrapper::new(listener, false);
            wrapper.set_verbose(VERBOSE);

            let tokens = lexer.tokens().filter_map(|(tok, ch, text, line, col)| {
                if ch == 0 {
                    if VERBOSE { println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\""); }
                    Some((tok, text, line, col))
                } else {
                    if VERBOSE { println!("TOKEN: channel {ch}, discarded, line {line} col {col}, Id {tok:?}, \"{text}\"")}
                    None
                }
            });
            let result = parser.parse_stream(&mut wrapper, tokens);
            assert_eq!(result, Ok(()));
        }
    }
}
