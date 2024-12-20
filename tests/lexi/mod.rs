use crate::gen::lexiparser::lexiparser::*;
use crate::gen::lexiparser::lexiparser_types::*;

struct LexiListener {}

impl LexiListener {
    fn new() -> Self {
        LexiListener {}
    }
}

impl LexiParserListener for LexiListener {
    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        todo!()
    }

    fn exit_file_item(&mut self, _ctx: CtxFileItem) -> SynFileItem {
        todo!()
    }

    fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader {
        todo!()
    }

    fn exit_declaration(&mut self, _ctx: CtxDeclaration) -> SynDeclaration {
        todo!()
    }

    fn exit_option(&mut self, _ctx: CtxOption) -> SynOption {
        todo!()
    }

    fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule {
        todo!()
    }

    fn exit_actions(&mut self, _ctx: CtxActions) -> SynActions {
        todo!()
    }

    fn exit_action(&mut self, _ctx: CtxAction) -> SynAction {
        todo!()
    }

    fn exit_match1(&mut self, _ctx: CtxMatch) -> SynMatch {
        todo!()
    }

    fn exit_alt_items(&mut self, _ctx: CtxAltItems) -> SynAltItems {
        todo!()
    }

    fn exit_alt_item(&mut self, _ctx: CtxAltItem) -> SynAltItem {
        todo!()
    }

    fn exit_repeat_item(&mut self, _ctx: CtxRepeatItem) -> SynRepeatItem {
        todo!()
    }

    fn exit_item(&mut self, _ctx: CtxItem) -> SynItem {
        todo!()
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
        let input = LEXICON;
        let stream = CharReader::new(Cursor::new(input));
        let mut lexer = build_lexer();
        lexer.attach_stream(stream);

        let parser = build_parser();
        let listener = LexiListener::new();
    }
}
