#[derive(Clone, Debug)]
pub enum LogMsg { Note(String), Warning(String), Error(String) }

#[derive(Clone, Debug)]
pub struct Logger {
    messages: Vec<LogMsg>,
    num_notes: usize,
    num_warnings: usize,
    num_errors: usize
}

impl Logger {
    pub fn new() -> Self {
        Logger { messages: Vec::new(), num_notes: 0, num_warnings: 0, num_errors: 0 }
    }

    pub fn clear(&mut self) {
        self.messages.clear();
        self.num_notes = 0;
        self.num_warnings = 0;
        self.num_errors = 0;
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

    pub fn num_notes(&self) -> usize {
        self.num_notes
    }

    pub fn num_warnings(&self) -> usize {
        self.num_warnings
    }

    pub fn num_errors(&self) -> usize {
        self.num_errors
    }

    pub fn add_note(&mut self, msg: String) {
        self.messages.push(LogMsg::Note(msg.to_string()));
        self.num_notes += 1;
    }

    pub fn add_warning(&mut self, msg: String) {
        self.messages.push(LogMsg::Warning(msg.to_string()));
        self.num_warnings += 1;
    }

    pub fn add_error(&mut self, msg: String) {
        self.messages.push(LogMsg::Error(msg.to_string()));
        self.num_errors += 1;
    }
}