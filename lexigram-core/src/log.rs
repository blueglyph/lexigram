// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fmt::{Debug, Display, Formatter};
// use crate::{BuildError, HasBuildErrorSource};

static NO_LOG_STORE: LogMsg = LogMsg::NoLogStore;

/// Common log functionalities for a message consumer/status verifyier
pub trait LogStatus: Debug {
    fn num_notes(&self) -> usize;
    fn num_warnings(&self) -> usize;
    fn num_errors(&self) -> usize;
    #[inline]
    fn has_no_errors(&self) -> bool {
        self.num_errors() == 0
    }
    #[inline]
    fn has_no_warnings(&self) -> bool {
        self.num_warnings() == 0
    }

    fn get_messages(&self) -> impl Iterator<Item = &LogMsg> {
        [&NO_LOG_STORE].into_iter() // should we panic instead?
    }

    fn get_messages_str(&self) -> String {
        self.get_messages().map(|m| format!("- {m}")).collect::<Vec<_>>().join("\n")
    }

    fn get_notes(&self) -> impl Iterator<Item = &String> {
        self.get_messages().filter_map(|m| if let LogMsg::Note(s) = m { Some(s) } else { None })
    }

    fn get_warnings(&self) -> impl Iterator<Item = &String> {
        self.get_messages().filter_map(|m| if let LogMsg::Warning(s) = m { Some(s) } else { None })
    }

    fn get_errors(&self) -> impl Iterator<Item = &String> {
        self.get_messages().filter_map(|m| if let LogMsg::Error(s) = m { Some(s) } else { None })
    }
}

/// Common log functionalities for a message producer
pub trait Logger: Debug {
    fn add_note<T: Into<String>>(&mut self, msg: T);
    fn add_warning<T: Into<String>>(&mut self, msg: T);
    fn add_error<T: Into<String>>(&mut self, msg: T);
}

// ---------------------------------------------------------------------------------------------

/// Basic log system that prints out messages to stderr without storing them
#[derive(Clone, Debug)]
pub struct PrintLog {
    num_notes: usize,
    num_warnings: usize,
    num_errors: usize
}

impl PrintLog {
    pub fn new() -> PrintLog {
        PrintLog { num_notes: 0, num_warnings: 0, num_errors: 0}
    }
}

impl LogStatus for PrintLog {
    fn num_notes(&self) -> usize {
        self.num_notes
    }

    fn num_warnings(&self) -> usize {
        self.num_warnings
    }

    fn num_errors(&self) -> usize {
        self.num_errors
    }
}

impl Logger for PrintLog {
    fn add_note<T: Into<String>>(&mut self, msg: T) {
        eprintln!("NOTE:    {}", msg.into());
    }

    fn add_warning<T: Into<String>>(&mut self, msg: T) {
        eprintln!("WARNING: {}", msg.into());
    }

    fn add_error<T: Into<String>>(&mut self, msg: T) {
        eprintln!("ERROR:   {}", msg.into());
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum LogMsg { NoLogStore, Note(String), Warning(String), Error(String) }

impl Display for LogMsg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LogMsg::NoLogStore => write!(f, "The log messages were not stored"),
            LogMsg::Note(s) =>    write!(f, "Note   : {s}"),
            LogMsg::Warning(s) => write!(f, "Warning: {s}"),
            LogMsg::Error(s) =>   write!(f, "ERROR  : {s}"),
        }
    }
}
/// Log system that stores the messages
#[derive(Clone, Debug)]
pub struct BufLog {
    messages: Vec<LogMsg>,
    num_notes: usize,
    num_warnings: usize,
    num_errors: usize
}

impl BufLog {
    pub fn new() -> Self {
        BufLog { messages: Vec::new(), num_notes: 0, num_warnings: 0, num_errors: 0 }
    }

    pub fn is_empty(&self) -> bool {
        self.messages.is_empty()
    }

    /// Clears all messages: notes, warnings, and errors.
    pub fn clear(&mut self) {
        self.messages.clear();
        self.num_notes = 0;
        self.num_warnings = 0;
        self.num_errors = 0;
    }

    /// Extends the messages with another Logger's messages.
    pub fn extend(&mut self, other: BufLog) {
        self.num_notes += other.num_notes;
        self.num_warnings += other.num_warnings;
        self.num_errors += other.num_errors;
        self.messages.extend(other.messages)
    }

    pub fn extend_messages<T: IntoIterator<Item = LogMsg>>(&mut self, iter: T) {
        self.messages.extend(iter.into_iter().inspect(|m| {
            match m {
                LogMsg::NoLogStore => {}
                LogMsg::Note(_) => self.num_notes += 1,
                LogMsg::Warning(_) => self.num_warnings += 1,
                LogMsg::Error(_) => self.num_errors += 1,
            }
        }));

    }
}

impl LogStatus for BufLog {
    fn num_notes(&self) -> usize {
        self.num_notes
    }

    fn num_warnings(&self) -> usize {
        self.num_warnings
    }

    fn num_errors(&self) -> usize {
        self.num_errors
    }

    fn get_messages(&self) -> impl Iterator<Item = &LogMsg> {
        self.messages.iter()
    }

    fn get_messages_str(&self) -> String {
        self.get_messages().map(|m| format!("- {m}")).collect::<Vec<_>>().join("\n")
    }

}

impl Logger for BufLog {
    fn add_note<T: Into<String>>(&mut self, msg: T) {
        self.messages.push(LogMsg::Note(msg.into()));
        self.num_notes += 1;
    }

    fn add_warning<T: Into<String>>(&mut self, msg: T) {
        self.messages.push(LogMsg::Warning(msg.into()));
        self.num_warnings += 1;
    }

    fn add_error<T: Into<String>>(&mut self, msg: T) {
        self.messages.push(LogMsg::Error(msg.into()));
        self.num_errors += 1;
    }
}

impl Default for BufLog {
    fn default() -> Self {
        BufLog::new()
    }
}

impl Display for BufLog {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_messages_str())
    }
}

// ---------------------------------------------------------------------------------------------
// blanket implementations: LogReader -> LogStatus, LogWriter -> Logger

pub trait LogReader {
    type Item: LogStatus;

    fn get_log(&self) -> &Self::Item;

    fn give_log(self) -> Self::Item;
}

pub trait LogWriter {
    fn get_mut_log(&mut self) -> &mut impl Logger;
}

impl<T: LogReader + Debug> LogStatus for T {
    fn num_notes(&self) -> usize {
        self.get_log().num_notes()
    }

    fn num_warnings(&self) -> usize {
        self.get_log().num_warnings()
    }

    fn num_errors(&self) -> usize {
        self.get_log().num_errors()
    }

    fn has_no_errors(&self) -> bool {
        self.get_log().has_no_errors()
    }

    fn has_no_warnings(&self) -> bool {
        self.get_log().has_no_warnings()
    }

    fn get_messages(&self) -> impl Iterator<Item=&LogMsg> {
        self.get_log().get_messages()
    }

    fn get_messages_str(&self) -> String {
        self.get_log().get_messages_str()
    }
}

impl<L: LogWriter + Debug> Logger for L {
    fn add_note<T: Into<String>>(&mut self, msg: T) {
        self.get_mut_log().add_note(msg);
    }

    fn add_warning<T: Into<String>>(&mut self, msg: T) {
        self.get_mut_log().add_warning(msg);
    }

    fn add_error<T: Into<String>>(&mut self, msg: T) {
        self.get_mut_log().add_error(msg);
    }
}

// ---------------------------------------------------------------------------------------------------------

