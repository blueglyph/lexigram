#![cfg(test)]

use rlexer::log::{Log, Logger};
use rlexer::segments::Segments;
use crate::out::lexiparser::lexiparser::*;
use crate::out::lexiparser::lexiparser_types::*;
use crate::lexi::LexAction;

struct LexiListener {
    verbose: bool,
    log: Log
}

impl LexiListener {
    fn new() -> Self {
        LexiListener {
            verbose: false,
            log: Log::new()
        }
    }
}

impl LexiParserListener for LexiListener {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

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
        SynActions(LexAction::default()) // dummy
    }

    fn init_action(&mut self) {
        if self.verbose { println!("init_action"); }
    }

    fn exit_action(&mut self, _ctx: CtxAction) -> SynAction {
        if self.verbose { println!("exit_action"); }
        SynAction(LexAction::default())
    }

    fn init_match(&mut self) {
        if self.verbose { println!("init_match1"); }
    }

    fn exit_match(&mut self, _ctx: CtxMatch) -> SynMatch {
        if self.verbose { println!("exit_match1"); }
        SynMatch(None)
    }

    fn init_alt_items(&mut self) {
        if self.verbose { println!("init_alt_items"); }
    }

    fn exit_alt_items(&mut self, _ctx: CtxAltItems) -> SynAltItems {
        if self.verbose { println!("exit_alt_items"); }
        SynAltItems((0, None))
    }

    fn init_alt_item(&mut self) {
        if self.verbose { println!("init_alt_item"); }
    }

    fn exit_alt_item(&mut self, _ctx: CtxAltItem) -> SynAltItem {
        if self.verbose { println!("exit_alt_item"); }
        SynAltItem((0, None))
    }

    fn init_repeat_item(&mut self) {
        if self.verbose { println!("init_repeat_item"); }
    }

    fn exit_repeat_item(&mut self, _ctx: CtxRepeatItem) -> SynRepeatItem {
        if self.verbose { println!("exit_repeat_item"); }
        SynRepeatItem((0, None))
    }

    fn init_item(&mut self) {
        if self.verbose { println!("init_item"); }
    }

    fn exit_item(&mut self, _ctx: CtxItem) -> SynItem {
        if self.verbose { println!("exit_item"); }
        SynItem((0, None))
    }

    fn exit_char_set(&mut self, _ctx: CtxCharSet) -> SynCharSet {
        SynCharSet(Segments::empty())
    }

    fn exit_char_set_one(&mut self, _ctx: CtxCharSetOne) -> SynCharSetOne {
        SynCharSetOne(Segments::empty())
    }
}

mod tests {
    use std::io::Cursor;
    use rlexer::CollectJoin;
    use rlexer::io::CharReader;
    use rlexer::lexi::LEXICON;
    use super::*;
    use crate::out::build_lexer;
    use crate::out::lexiparser::lexiparser::build_parser;
    use rlexer::lexer::TokenSpliterator;

    #[test]
    fn lexer_parser() {
        let tests = [
            ("", 0),
            ("lexicon LexiLexer;", 3),
            (LEXICON, 327),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_DETAILS: bool = false;
        const VERBOSE_LISTENER: bool = false;

        for method in 0..3 {
            for (test_id, (input, expected_tokens)) in tests.into_iter().enumerate() {
                if VERBOSE { println!("// {:=<80}\n// Test {test_id}", ""); }
                let stream = CharReader::new(Cursor::new(input));
                let mut lexer = build_lexer();
                lexer.set_tab_width(4);
                lexer.attach_stream(stream);

                let mut parser = build_parser();
                let mut listener = LexiListener::new();
                listener.verbose = VERBOSE_LISTENER;
                let mut wrapper = ListenerWrapper::new(listener, false);
                wrapper.set_verbose(VERBOSE);
                let mut result_tokens = 0;
                let result = match method {
                    // 'match' returns the result and not the tokens because the token iterator has a different
                    // type in each case:
                    0 => {
                        let tokens = lexer.tokens().split_channels(0, |(_tok, ch, text, line, col)|
                            panic!("no channel {ch} in this test, line {line} col {col}, \"{text}\""),
                        ).inspect(|(tok, text, line, col)| {
                            result_tokens += 1;
                            if VERBOSE_DETAILS { println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\""); }
                        });
                        parser.parse_stream(&mut wrapper, tokens)
                    }
                    1 => {
                        let tokens = lexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
                            panic!("no channel {ch} in this test, line {line} col {col}, \"{text}\"")
                        ).inspect(|(tok, text, line, col)| {
                            result_tokens += 1;
                            if VERBOSE_DETAILS { println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\""); }
                        });
                        parser.parse_stream(&mut wrapper, tokens)
                    }
                    2 => parser.parse_stream(&mut wrapper, lexer.tokens().keep_channel0().inspect(|_| result_tokens += 1)),
                    3 => parser.parse_stream(&mut wrapper, lexer.tokens().keep_channel(0).inspect(|_| result_tokens += 1)),
                    _ => panic!()
                };
                if VERBOSE {
                    let msg = wrapper.get_mut_listener().log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                    if !msg.is_empty() {
                        println!("Messages:\n{msg}");
                    }
                }
                let text = format!("test {test_id}, method {method} failed");
                assert_eq!(result, Ok(()), "{text}");
                assert_eq!(result_tokens, expected_tokens, "{text}");
            }
        }
    }
}
