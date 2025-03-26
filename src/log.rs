#[derive(Clone, Debug)]
pub enum LogMsg { Note(String), Warning(String), Error(String) }

#[derive(Clone, Debug)]
pub struct Log {
    messages: Vec<LogMsg>,
    num_notes: usize,
    num_warnings: usize,
    num_errors: usize
}

impl Log {
    pub fn new() -> Self {
        Log { messages: Vec::new(), num_notes: 0, num_warnings: 0, num_errors: 0 }
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
    pub fn extend(&mut self, other: Log) {
        self.num_notes += other.num_notes;
        self.num_warnings += other.num_warnings;
        self.num_errors += other.num_errors;
        self.messages.extend(other.messages)
    }

    pub fn get_messages(&self) -> impl Iterator<Item = &LogMsg> {
        self.messages.iter()
    }

    pub fn get_notes(&self) -> impl Iterator<Item = &String> {
        self.messages.iter().filter_map(|m| if let LogMsg::Note(s) = m { Some(s) } else { None })
    }

    pub fn get_warnings(&self) -> impl Iterator<Item = &String> {
        self.messages.iter().filter_map(|m| if let LogMsg::Warning(s) = m { Some(s) } else { None })
    }

    pub fn get_errors(&self) -> impl Iterator<Item = &String> {
        self.messages.iter().filter_map(|m| if let LogMsg::Error(s) = m { Some(s) } else { None })
    }
}

pub trait Logger {
    fn add_note<T: Into<String>>(&mut self, msg: T);
    fn add_warning<T: Into<String>>(&mut self, msg: T);
    fn add_error<T: Into<String>>(&mut self, msg: T);
    fn num_notes(&self) -> usize;
    fn num_warnings(&self) -> usize;
    fn num_errors(&self) -> usize;
    // fn get_notes(&self) -> impl Iterator<Item = &String>;
    // fn get_warnings(&self) -> impl Iterator<Item = &String>;
    // fn get_errors(&self) -> impl Iterator<Item = &String>;
}

impl Logger for Log {
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