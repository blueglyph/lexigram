// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#[cfg(test)]
use lexigram_core::{lexer::PosSpan, log::LogMsg, text_span::{GetLine, GetTextSpan}};

pub mod lalr;
pub mod ll1;

#[cfg(test)]
fn transform_msg<T: GetLine>(listener: &mut T, span_opt: Option<&PosSpan>, msg: &mut LogMsg) {
    if let Some(span) = span_opt {
        match msg {
            LogMsg::NoLogStore => {}
            LogMsg::Note(s)
            | LogMsg::Info(s)
            | LogMsg::Warning(s)
            | LogMsg::Error(s) => {
                *s = format!("{s}:\n{}", listener.annotate_text(&span))
            }
        }
    }
}
