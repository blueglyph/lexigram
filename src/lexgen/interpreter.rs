use std::fmt::{Display, Formatter};
use crate::dfa::{StateId, Token};
use crate::lexgen::{char_to_group, GroupId, LexGen};

// ---------------------------------------------------------------------------------------------
// Table-based lexer interpreter

pub struct SimLexGenError {
    pub pos: u64,
    pub curr_char: Option<char>,
    pub group: Option<GroupId>,
    pub token: Option<Token>,
    pub state: StateId,
    pub msg: String
}

impl Display for SimLexGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "pos: {}{}{}{}, state {}: {}",
            self.pos,
            self.curr_char.map(|c| format!(" on '{c}'")).unwrap_or("".to_string()),
            self.group.map(|g| format!(", group {g}")).unwrap_or("".to_string()),
            self.token.as_ref().map(|t| format!(", token {}", t.0)).unwrap_or("".to_string()),
            self.state,
            self.msg
        )
    }
}

pub fn interpret_lexgen(lexgen: &LexGen, input: String) -> Result<Token, SimLexGenError> {
    const VERBOSE: bool = false;
    let mut state = lexgen.initial_state;
    let mut chars = input.chars();
    let mut pos = 0_u64;
    loop {
        if VERBOSE { print!("- state = {state}"); }
        if let Some(c) = chars.next() {
            let group = char_to_group(&lexgen.ascii_to_group, &lexgen.utf8_to_group, c);
            if VERBOSE { print!(", char '{c}' -> group {group}"); }
            // we can use the state_table even if group = error = nrb_group (but we must
            // ignore new_state and detect that the group is illegal):
            let new_state = lexgen.state_table[lexgen.nbr_groups * state + group];
            if group >= lexgen.nbr_groups || new_state >= lexgen.nbr_states {
                if VERBOSE { println!(" <invalid input>"); }
                return Err(SimLexGenError {
                    pos,
                    curr_char: Some(c),
                    group: Some(group),
                    token: if state >= lexgen.first_end_state { Some(lexgen.token_table[state - lexgen.first_end_state].clone()) } else { None },
                    state,
                    msg: (if group >= lexgen.nbr_groups { "unrecognized character" } else { "unexpected character" }).to_string(),
                });
            }
            if VERBOSE { println!(" -> state {new_state}"); }
            state = new_state;
        } else {
            if VERBOSE { println!(" <end of input>"); }
            return if state >= lexgen.first_end_state {
                Ok(lexgen.token_table[state - lexgen.first_end_state].clone())
            } else {
                Err(SimLexGenError {
                    pos,
                    curr_char: None,
                    group: None,
                    token: None,
                    state,
                    msg: "unexpected end of stream".to_string(),
                })
            };
        }
        pos += 1;
    }
}

