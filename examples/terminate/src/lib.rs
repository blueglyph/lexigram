// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

mod ll1;
mod lalr;

static TXT1: &str = r#"
## ERROR: discard this
PROCESS A1
## NOTE: first note
## WARNING: first warning
END A1

PROCESS B1
## ERROR: first error
END B1
"#;

static TXT2: &str = r#"
## NOTE: previous process, to discard

PROCESS C1
END C1
PROCESS C2
## NOTE: start a
## NOTE: start b
## WARNING: limit reached
END C2

PROCESS C3
## NOTE: start x

## ERROR: parameter is out of range
SHUTDOWN
## ERROR: irrelevant message

PROCESS D1
"#;

static TXT3: &str = r#"
PROCESS D1
## NOTE: OK so far
END D2
"#;

