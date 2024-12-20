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
    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        if self.verbose { println!("exit_file"); }
        SynFile()
    }

    fn exit_file_item(&mut self, _ctx: CtxFileItem) -> SynFileItem {
        if self.verbose { println!("exit_file_item"); }
        SynFileItem()
    }

    fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader {
        if self.verbose { println!("exit_header"); }
        SynHeader()
    }

    fn exit_declaration(&mut self, _ctx: CtxDeclaration) -> SynDeclaration {
        if self.verbose { println!("exit_declaration"); }
        SynDeclaration()
    }

    fn exit_option(&mut self, _ctx: CtxOption) -> SynOption {
        if self.verbose { println!("exit_option"); }
        SynOption()
    }

    fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule {
        if self.verbose { println!("exit_rule"); }
        SynRule()
    }

    fn exit_actions(&mut self, _ctx: CtxActions) -> SynActions {
        if self.verbose { println!("exit_actions"); }
        SynActions()
    }

    fn exit_action(&mut self, _ctx: CtxAction) -> SynAction {
        if self.verbose { println!("exit_action"); }
        SynAction()
    }

    fn exit_match1(&mut self, _ctx: CtxMatch) -> SynMatch {
        if self.verbose { println!("exit_match1"); }
        SynMatch()
    }

    fn exit_alt_items(&mut self, _ctx: CtxAltItems) -> SynAltItems {
        if self.verbose { println!("exit_alt_items"); }
        SynAltItems()
    }

    fn exit_alt_item(&mut self, _ctx: CtxAltItem) -> SynAltItem {
        if self.verbose { println!("exit_alt_item"); }
        SynAltItem()
    }

    fn exit_repeat_item(&mut self, _ctx: CtxRepeatItem) -> SynRepeatItem {
        if self.verbose { println!("exit_repeat_item"); }
        SynRepeatItem()
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
            lexer.attach_stream(stream);

            let mut parser = build_parser();
            let mut listener = LexiListener::new();
            listener.verbose = VERBOSE;
            let mut wrapper = ListenerWrapper::new(listener, false);
            wrapper.set_verbose(VERBOSE);

            let tokens = lexer.tokens().filter_map(|(tok, ch, text)| if ch == 0 { Some((tok, text)) } else { None });
            let result = parser.parse_stream(&mut wrapper, tokens);
        }
    }
}
