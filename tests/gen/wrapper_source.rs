#![cfg(test)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

// ================================================================================
// Test 0: rules PRS(34) #1, start 0:
/*
before, NT with value: S, VAL
after,  NT with value: S, VAL
            // NT flags:
            //  - (nothing)
            // parents:
            //  - (nothing)
            (PRS(34), 0, btreemap![
                0 => symbols![t 0, nt 1],               //  0: S -> id = VAL   | ◄0 ►VAL = id!  | id VAL
                1 => symbols![],                        //  1: S -> exit       | ◄1 exit        |
                2 => symbols![nt 1],                    //  2: S -> return VAL | ◄2 ►VAL return | VAL
                3 => symbols![t 0],                     //  3: VAL -> id       | ◄3 id!         | id
                4 => symbols![t 1],                     //  4: VAL -> num      | ◄4 num!        | num
            ], Set(symbols![nt 0, nt 1, t 0, t 1]), btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),
*/
mod rules_prs_34_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(34) #1, start S]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { S { s: SynS } }
    #[derive(Debug)]
    pub enum CtxS {
        /// S -> id = VAL
        S1 { id: String, val: SynVal },
        /// S -> exit
        S2,
        /// S -> return VAL
        S3 { val: SynVal },
    }
    #[derive(Debug)]
    pub enum CtxVal {
        /// VAL -> id
        Val1 { id: String },
        /// VAL -> num
        Val2 { num: String },
    }

    // User-defined: SynS, SynVal
    #[derive(Debug)]
    pub struct SynS();
    #[derive(Debug)]
    pub struct SynVal();

    #[derive(Debug)]
    enum SynValue { S(SynS), Val(SynVal) }

    impl SynValue {
        fn get_s(self) -> SynS {
            if let SynValue::S(val) = self { val } else { panic!() }
        }
        fn get_val(self) -> SynVal {
            if let SynValue::Val(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_s(&mut self) {}
        fn exit_s(&mut self, _ctx: CtxS) -> SynS;
        fn init_val(&mut self) {}
        fn exit_val(&mut self, _ctx: CtxVal) -> SynVal;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_s(),                // S
                        1 => self.listener.init_val(),              // VAL
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 |                                         // S -> id = VAL
                        1 |                                         // S -> exit
                        2 => self.exit_s(factor_id),                // S -> return VAL
                        3 |                                         // VAL -> id
                        4 => self.exit_val(factor_id),              // VAL -> num
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let s = self.stack.pop().unwrap().get_s();
            self.listener.exit(Ctx::S { s });
        }
        fn exit_s(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                0 => {
                    let val = self.stack.pop().unwrap().get_val();
                    let id = self.stack_t.pop().unwrap();
                    CtxS::S1 { id, val }
                }
                1 => {
                    CtxS::S2
                }
                2 => {
                    let val = self.stack.pop().unwrap().get_val();
                    CtxS::S3 { val }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_s")
            };
            let val = self.listener.exit_s(ctx);
            self.stack.push(SynValue::S(val));
        }
        fn exit_val(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                3 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxVal::Val1 { id }
                }
                4 => {
                    let num = self.stack_t.pop().unwrap();
                    CtxVal::Val2 { num }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_val")
            };
            let val = self.listener.exit_val(ctx);
            self.stack.push(SynValue::Val(val));
        }
    }

    // [wrapper source for rule PRS(34) #1, start S]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 1: rules RTS(21) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A, A_1
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(21), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], All, btreemap![0 => vec![0]]),
*/
mod rules_rts_21_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(21) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_1 c
        A { a: String, star: SynA1, c: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<String>);
    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA), A1(SynA1) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => self.init_a1(),                        // A_1
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_1 c
                        1 => self.exit_a1(),                        // A_1 -> b A_1
                        2 => {}                                     // A_1 -> ε
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let star = self.stack.pop().unwrap().get_a1();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, star, c });
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let mut star_it = self.stack.pop().unwrap().get_a1();
            star_it.0.push(b);
            self.stack.push(SynValue::A1(star_it));
        }
    }

    // [wrapper source for rule RTS(21) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 2: rules RTS(21) #2, start 0:
/*
before, NT with value: A
after,  NT with value: A
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(21), 0, btreemap![
                0 => symbols![t 0, t 2],                //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a c
                1 => symbols![],                        //  1: A_1 -> b A_1 | ●A_1 ◄1 b     |
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], Set(symbols![nt 0, t 0, t 2]), btreemap![0 => vec![0]]),
*/
mod rules_rts_21_2 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(21) #2, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_1 c
        A { a: String, c: String },
    }

    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => {}                                     // A_1
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_1 c
                        1 |                                         // A_1 -> b A_1
                        2 => {}                                     // A_1 -> ε
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, c });
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule RTS(21) #2, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 3: rules RTS(22) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A, A_1
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* | L-form (129)
            // parents:
            //  - A_1 -> A
            (RTS(22), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], All, btreemap![0 => vec![0]]),
*/
mod rules_rts_22_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(22) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_1 c
        A { a: String, star: SynAIter, c: String },
    }
    #[derive(Debug)]
    pub enum CtxAIter {
        /// A_1 -> b A_1
        A1 { star_it: SynAIter, b: String },
    }

    // User-defined: SynA, SynAIter
    #[derive(Debug)]
    pub struct SynA();
    #[derive(Debug)]
    pub struct SynAIter();

    #[derive(Debug)]
    enum SynValue { A(SynA), AIter(SynAIter) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_a_iter(self) -> SynAIter {
            if let SynValue::AIter(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_a_iter(&mut self) -> SynAIter;
        fn exit_a_iter(&mut self, _ctx: CtxAIter) -> SynAIter;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => self.init_a_iter(),                    // A_1
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_1 c
                        1 => self.exit_a_iter(),                    // A_1 -> b A_1
                        2 => {}                                     // A_1 -> ε
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let star = self.stack.pop().unwrap().get_a_iter();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, star, c });
            self.stack.push(SynValue::A(val));
        }
        fn init_a_iter(&mut self) {
            let val = self.listener.init_a_iter();
            self.stack.push(SynValue::AIter(val));
        }
        fn exit_a_iter(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let star_it = self.stack.pop().unwrap().get_a_iter();
            let val = self.listener.exit_a_iter(CtxAIter::A1 { star_it, b });
            self.stack.push(SynValue::AIter(val));
        }
    }

    // [wrapper source for rule RTS(22) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 4: rules RTS(22) #2, start 0:
/*
before, NT with value: A
after,  NT with value: A
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* | L-form (129)
            // parents:
            //  - A_1 -> A
            (RTS(22), 0, btreemap![
                0 => symbols![t 0, t 2],                //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a c
                1 => symbols![],                        //  1: A_1 -> b A_1 | ●A_1 ◄1 b     |
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], Set(symbols![nt 0, t 0, t 2]), btreemap![0 => vec![0]]),
*/
mod rules_rts_22_2 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(22) #2, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_1 c
        A { a: String, c: String },
    }

    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_a_iter(&mut self) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => self.listener.init_a_iter(),           // A_1
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_1 c
                        1 |                                         // A_1 -> b A_1
                        2 => {}                                     // A_1 -> ε
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, c });
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule RTS(22) #2, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 5: rules RTS(32) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A, A_1
            // NT flags:
            //  - A: parent_left_fact | parent_+_or_* (2080)
            //  - A_1: child_+_or_* | L-form (129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (RTS(32), 0, btreemap![
                0 => symbols![],                        //  0: A -> a A_2     | ►A_2 a!       |
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1   | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε       | ◄2            |
                3 => symbols![t 0, t 0, nt 1, t 2],     //  3: A_2 -> a A_1 c | ◄3 c! ►A_1 a! | a a A_1 c
                4 => symbols![t 0, t 2, nt 1, t 2],     //  4: A_2 -> c A_1 c | ◄4 c! ►A_1 c! | a c A_1 c
            ], All, btreemap![0 => vec![3, 4]]),
*/
mod rules_rts_32_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(32) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a a A_1 c
        A1 { a: [String; 2], star: SynAIter, c: String },
        /// A -> a c A_1 c
        A2 { a: String, c: [String; 2], star: SynAIter },
    }
    #[derive(Debug)]
    pub enum CtxAIter {
        /// A_1 -> b A_1
        A1 { star_it: SynAIter, b: String },
    }

    // User-defined: SynA, SynAIter
    #[derive(Debug)]
    pub struct SynA();
    #[derive(Debug)]
    pub struct SynAIter();

    #[derive(Debug)]
    enum SynValue { A(SynA), AIter(SynAIter) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_a_iter(self) -> SynAIter {
            if let SynValue::AIter(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_a_iter(&mut self) -> SynAIter;
        fn exit_a_iter(&mut self, _ctx: CtxAIter) -> SynAIter;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => self.init_a_iter(),                    // A_1
                        2 => {}                                     // A_2
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        3 |                                         // A -> a a A_1 c
                        4 => self.exit_a(factor_id),                // A -> a c A_1 c
                        1 => self.exit_a_iter(),                    // A_1 -> b A_1
                        2 => {}                                     // A_1 -> ε
                     /* 0 */                                        // A -> a a A_1 c | a c A_1 c (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                3 => {
                    let c = self.stack_t.pop().unwrap();
                    let star = self.stack.pop().unwrap().get_a_iter();
                    let a_2 = self.stack_t.pop().unwrap();
                    let a_1 = self.stack_t.pop().unwrap();
                    CtxA::A1 { a: [a_1, a_2], star, c }
                }
                4 => {
                    let c_2 = self.stack_t.pop().unwrap();
                    let star = self.stack.pop().unwrap().get_a_iter();
                    let c_1 = self.stack_t.pop().unwrap();
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A2 { a, c: [c_1, c_2], star }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
        fn init_a_iter(&mut self) {
            let val = self.listener.init_a_iter();
            self.stack.push(SynValue::AIter(val));
        }
        fn exit_a_iter(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let star_it = self.stack.pop().unwrap().get_a_iter();
            let val = self.listener.exit_a_iter(CtxAIter::A1 { star_it, b });
            self.stack.push(SynValue::AIter(val));
        }
    }

    // [wrapper source for rule RTS(32) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 6: rules RTS(25) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(25), 0, btreemap![
                0 => symbols![t 0, t 2],                //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a c
                1 => symbols![],                        //  1: A_1 -> # A_1 | ●A_1 ◄1 #     |
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], Default, btreemap![0 => vec![0]]),
*/
mod rules_rts_25_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(25) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_1 c
        A { a: String, c: String },
    }

    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => {}                                     // A_1
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_1 c
                        1 |                                         // A_1 -> # A_1
                        2 => {}                                     // A_1 -> ε
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, c });
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule RTS(25) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 7: rules RTS(23) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A, A_1
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(23), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![],                        //  1: A_1 -> b A_2 | ►A_2 b!       |
                2 => symbols![nt 1, t 1],               //  2: A_2 -> A_1   | ●A_1 ◄2       | A_1 b
                3 => symbols![nt 1, t 1],               //  3: A_2 -> ε     | ◄3            | A_1 b
            ], All, btreemap![0 => vec![0]]),
*/
mod rules_rts_23_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(23) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_1 c
        A { a: String, plus: SynA1, c: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<String>);
    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA), A1(SynA1) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => self.init_a1(),                        // A_1
                        2 => {}                                     // A_2
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_1 c
                        2 |                                         // A_1 -> b A_1
                        3 => self.exit_a1(),                        // A_1 -> b
                     /* 1 */                                        // A_1 -> b | b A_1 (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a1();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, plus, c });
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let mut plus_it = self.stack.pop().unwrap().get_a1();
            plus_it.0.push(b);
            self.stack.push(SynValue::A1(plus_it));
        }
    }

    // [wrapper source for rule RTS(23) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 8: rules RTS(27) #1, start 0:
/*
before, NT with value: A, B
after,  NT with value: A, B, A_1
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(27), 0, btreemap![
                0 => symbols![t 0, nt 2, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![t 1],                     //  1: B -> b       | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B A_2 | ►A_2 ►B       |
                3 => symbols![nt 2, nt 1],              //  3: A_2 -> A_1   | ●A_1 ◄3       | A_1 B
                4 => symbols![nt 2, nt 1],              //  4: A_2 -> ε     | ◄4            | A_1 B
            ], All, btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_rts_27_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(27) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_1 c
        A { a: String, plus: SynA1, c: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// B -> b
        B { b: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<SynA1Item>);
    #[derive(Debug)]
    pub struct SynA1Item { b: SynB }
    // User-defined: SynA, SynB
    #[derive(Debug)]
    pub struct SynA();
    #[derive(Debug)]
    pub struct SynB();

    #[derive(Debug)]
    enum SynValue { A(SynA), B(SynB), A1(SynA1) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_b(self) -> SynB {
            if let SynValue::B(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_b(&mut self) {}
        fn exit_b(&mut self, _ctx: CtxB) -> SynB;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        2 => self.init_a1(),                        // A_1
                        3 => {}                                     // A_2
                        1 => self.listener.init_b(),                // B
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_1 c
                        3 |                                         // A_1 -> B A_1
                        4 => self.exit_a1(),                        // A_1 -> B
                     /* 2 */                                        // A_1 -> B | B A_1 (never called)
                        1 => self.exit_b(),                         // B -> b
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a1();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, plus, c });
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let b = self.stack.pop().unwrap().get_b();
            let mut plus_it = self.stack.pop().unwrap().get_a1();
            plus_it.0.push(SynA1Item { b });
            self.stack.push(SynValue::A1(plus_it));
        }
        fn exit_b(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let val = self.listener.exit_b(CtxB::B { b });
            self.stack.push(SynValue::B(val));
        }
    }

    // [wrapper source for rule RTS(27) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 9: rules RTS(28) #1, start 0:
/*
before, NT with value: A, B
after,  NT with value: A, B, A_1
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(28), 0, btreemap![
                0 => symbols![nt 2, t 2],               //  0: A -> A_1 c     | ◄0 c! ►A_1 | A_1 c
                1 => symbols![t 1],                     //  1: B -> b         | ◄1 b!      | b
                2 => symbols![],                        //  2: A_1 -> a B A_2 | ►A_2 ►B a! |
                3 => symbols![nt 2, t 0, nt 1],         //  3: A_2 -> A_1     | ●A_1 ◄3    | A_1 a B
                4 => symbols![nt 2, t 0, nt 1],         //  4: A_2 -> ε       | ◄4         | A_1 a B
            ], All, btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_rts_28_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(28) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> A_1 c
        A { plus: SynA1, c: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// B -> b
        B { b: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<SynA1Item>);
    #[derive(Debug)]
    pub struct SynA1Item { a: String, b: SynB }
    // User-defined: SynA, SynB
    #[derive(Debug)]
    pub struct SynA();
    #[derive(Debug)]
    pub struct SynB();

    #[derive(Debug)]
    enum SynValue { A(SynA), B(SynB), A1(SynA1) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_b(self) -> SynB {
            if let SynValue::B(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_b(&mut self) {}
        fn exit_b(&mut self, _ctx: CtxB) -> SynB;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        2 => self.init_a1(),                        // A_1
                        3 => {}                                     // A_2
                        1 => self.listener.init_b(),                // B
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> A_1 c
                        3 |                                         // A_1 -> a B A_1
                        4 => self.exit_a1(),                        // A_1 -> a B
                     /* 2 */                                        // A_1 -> a B | a B A_1 (never called)
                        1 => self.exit_b(),                         // B -> b
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a1();
            let val = self.listener.exit_a(CtxA::A { plus, c });
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let b = self.stack.pop().unwrap().get_b();
            let a = self.stack_t.pop().unwrap();
            let mut plus_it = self.stack.pop().unwrap().get_a1();
            plus_it.0.push(SynA1Item { a, b });
            self.stack.push(SynValue::A1(plus_it));
        }
        fn exit_b(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let val = self.listener.exit_b(CtxB::B { b });
            self.stack.push(SynValue::B(val));
        }
    }

    // [wrapper source for rule RTS(28) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 10: rules RTS(29) #1, start 0:
/*
before, NT with value: A, B
after,  NT with value: A, B, A_1, A_2

// A -> a ( (B b)* c)* d

item_info =
[
  //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
  [
    ItemInfo { name: "a", sym: T(0), owner: 0, is_vec: false, index: None },
    ItemInfo { name: "star", sym: NT(3), owner: 0, is_vec: false, index: None },
    ItemInfo { name: "d", sym: T(3), owner: 0, is_vec: false, index: None }
  ],
  //  1: B -> b           | ◄1 b!           | b
  [
    ItemInfo { name: "b", sym: T(1), owner: 1, is_vec: false, index: None }
  ],
  //  2: A_1 -> B b A_1   | ●A_1 ◄2 b! ►B   | A_1 B b
  [
    ItemInfo { name: "star_it", sym: NT(2), owner: 2, is_vec: false, index: None },
    ItemInfo { name: "b", sym: NT(1), owner: 2, is_vec: false, index: None },
    ItemInfo { name: "b1", sym: T(1), owner: 2, is_vec: false, index: None }
  ],
  //  3: A_1 -> ε         | ◄3              |
  [],
  //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 A_1 c
  [
    ItemInfo { name: "star_it", sym: NT(3), owner: 3, is_vec: false, index: None },
    ItemInfo { name: "star", sym: NT(2), owner: 3, is_vec: false, index: None },
    ItemInfo { name: "c", sym: T(2), owner: 3, is_vec: false, index: None }
  ],
  //  5: A_2 -> ε         | ◄5              |
  []
]
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - A_1 -> A_2
            //  - A_2 -> A
            (RTS(29), 0, btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!           | b
                2 => symbols![nt 2, nt 1, t 1],         //  2: A_1 -> B b A_1   | ●A_1 ◄2 b! ►B   | A_1 B b
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, nt 2, t 2],         //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 A_1 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], All, btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_rts_29_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(29) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_2 d
        A { a: String, star: SynA2, d: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// B -> b
        B { b: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<SynA1Item>);
    #[derive(Debug)]
    pub struct SynA1Item { b: SynB, b1: String }
    #[derive(Debug)]
    pub struct SynA2(Vec<SynA2Item>);
    #[derive(Debug)]
    pub struct SynA2Item { star: SynA1, c: String }
    // User-defined: SynA, SynB
    #[derive(Debug)]
    pub struct SynA();
    #[derive(Debug)]
    pub struct SynB();

    #[derive(Debug)]
    enum SynValue { A(SynA), B(SynB), A1(SynA1), A2(SynA2) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_b(self) -> SynB {
            if let SynValue::B(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
        fn get_a2(self) -> SynA2 {
            if let SynValue::A2(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_b(&mut self) {}
        fn exit_b(&mut self, _ctx: CtxB) -> SynB;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        2 => self.init_a1(),                        // A_1
                        3 => self.init_a2(),                        // A_2
                        1 => self.listener.init_b(),                // B
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_2 d
                        2 => self.exit_a1(),                        // A_1 -> B b A_1
                        3 => {}                                     // A_1 -> ε
                        4 => self.exit_a2(),                        // A_2 -> A_1 c A_2
                        5 => {}                                     // A_2 -> ε
                        1 => self.exit_b(),                         // B -> b
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let d = self.stack_t.pop().unwrap();
            let star = self.stack.pop().unwrap().get_a2();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, star, d });
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let b1 = self.stack_t.pop().unwrap();
            let b = self.stack.pop().unwrap().get_b();
            let mut star_it = self.stack.pop().unwrap().get_a1();
            star_it.0.push(SynA1Item { b, b1 });
            self.stack.push(SynValue::A1(star_it));
        }
        fn init_a2(&mut self) {
            let val = SynA2(Vec::new());
            self.stack.push(SynValue::A2(val));
        }
        fn exit_a2(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let star = self.stack.pop().unwrap().get_a1();
            let mut star_it = self.stack.pop().unwrap().get_a2();
            star_it.0.push(SynA2Item { star, c });
            self.stack.push(SynValue::A2(star_it));
        }
        fn exit_b(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let val = self.listener.exit_b(CtxB::B { b });
            self.stack.push(SynValue::B(val));
        }
    }

    // [wrapper source for rule RTS(29) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 11: rules RTS(29) #2, start 0:
/*
before, NT with value: A
after,  NT with value: A, A_2
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - A_1 -> A_2
            //  - A_2 -> A
            (RTS(29), 0, btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![],                        //  1: B -> b           | ◄1 b            |
                2 => symbols![],                        //  2: A_1 -> B b A_1   | ●A_1 ◄2 b ►B    |
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, t 2],               //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], Set(symbols![nt 0, t 0, t 2, t 3]), btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_rts_29_2 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(29) #2, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_2 d
        A { a: String, star: SynA2, d: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// B -> b
        B,
    }

    #[derive(Debug)]
    pub struct SynA2(Vec<String>);
    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA), A2(SynA2) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_a2(self) -> SynA2 {
            if let SynValue::A2(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_b(&mut self) {}
        fn exit_b(&mut self, _ctx: CtxB) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        2 => {}                                     // A_1
                        3 => self.init_a2(),                        // A_2
                        1 => self.listener.init_b(),                // B
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_2 d
                        2 |                                         // A_1 -> B b A_1
                        3 => {}                                     // A_1 -> ε
                        4 => self.exit_a2(),                        // A_2 -> A_1 c A_2
                        5 => {}                                     // A_2 -> ε
                        1 => self.exit_b(),                         // B -> b
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let d = self.stack_t.pop().unwrap();
            let star = self.stack.pop().unwrap().get_a2();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, star, d });
            self.stack.push(SynValue::A(val));
        }
        fn init_a2(&mut self) {
            let val = SynA2(Vec::new());
            self.stack.push(SynValue::A2(val));
        }
        fn exit_a2(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let mut star_it = self.stack.pop().unwrap().get_a2();
            star_it.0.push(c);
            self.stack.push(SynValue::A2(star_it));
        }
        fn exit_b(&mut self) {
            self.listener.exit_b(CtxB::B);
        }
    }

    // [wrapper source for rule RTS(29) #2, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 12: rules RTS(29) #3, start 0:
/*
before, NT with value:
after,  NT with value: A_1, A_2
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - A_1 -> A_2
            //  - A_2 -> A
            (RTS(29), 0, btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!           | b
                2 => symbols![nt 2, t 1],               //  2: A_1 -> B b A_1   | ●A_1 ◄2 b! ►B   | A_1 b
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, nt 2, t 2],         //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 A_1 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], Set(symbols![t 0, t 1, t 2, t 3]), btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_rts_29_3 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(29) #3, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A } // A has no value: nothing returned from the top non-terminal
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_2 d
        A { a: String, star: SynA2, d: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// B -> b
        B { b: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<String>);
    #[derive(Debug)]
    pub struct SynA2(Vec<SynA2Item>);
    #[derive(Debug)]
    pub struct SynA2Item { star: SynA1, c: String }
    // Top non-terminal A has no value:
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A1(SynA1), A2(SynA2) }

    impl SynValue {
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
        fn get_a2(self) -> SynA2 {
            if let SynValue::A2(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) {}
        fn init_b(&mut self) {}
        fn exit_b(&mut self, _ctx: CtxB) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        2 => self.init_a1(),                        // A_1
                        3 => self.init_a2(),                        // A_2
                        1 => self.listener.init_b(),                // B
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_2 d
                        2 => self.exit_a1(),                        // A_1 -> B b A_1
                        3 => {}                                     // A_1 -> ε
                        4 => self.exit_a2(),                        // A_2 -> A_1 c A_2
                        5 => {}                                     // A_2 -> ε
                        1 => self.exit_b(),                         // B -> b
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            self.listener.exit(Ctx::A);
        }
        fn exit_a(&mut self) {
            let d = self.stack_t.pop().unwrap();
            let star = self.stack.pop().unwrap().get_a2();
            let a = self.stack_t.pop().unwrap();
            self.listener.exit_a(CtxA::A { a, star, d });
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let mut star_it = self.stack.pop().unwrap().get_a1();
            star_it.0.push(b);
            self.stack.push(SynValue::A1(star_it));
        }
        fn init_a2(&mut self) {
            let val = SynA2(Vec::new());
            self.stack.push(SynValue::A2(val));
        }
        fn exit_a2(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let star = self.stack.pop().unwrap().get_a1();
            let mut star_it = self.stack.pop().unwrap().get_a2();
            star_it.0.push(SynA2Item { star, c });
            self.stack.push(SynValue::A2(star_it));
        }
        fn exit_b(&mut self) {
            let b = self.stack_t.pop().unwrap();
            self.listener.exit_b(CtxB::B { b });
        }
    }

    // [wrapper source for rule RTS(29) #3, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 13: rules RTS(30) #1, start 0:
/*
before, NT with value: A, B
after,  NT with value: A, B, A_1, A_2
nt_name = [Some(("A", "a")), Some(("B", "b")), Some(("A1", "a1")), Some(("A2", "a2")), None, None]
item_info =
[
  //  0: A -> a A_2 d     | ◄0 d! ►A_2 a! | a A_2 d
  [
    ItemInfo { name: "a", sym: T(0), owner: 0, is_vec: false, index: None },
    ItemInfo { name: "plus", sym: NT(3), owner: 0, is_vec: false, index: None },
    ItemInfo { name: "d", sym: T(3), owner: 0, is_vec: false, index: None }
  ],
  //  1: B -> b           | ◄1 b!         | b
  [
    ItemInfo { name: "b", sym: T(1), owner: 1, is_vec: false, index: None }
  ],
  //  2: A_1 -> B b A_3   | ►A_3 b! ►B    |
  [],
  //  3: A_2 -> A_1 c A_4 | ►A_4 c! ►A_1  |
  [],
  //  4: A_3 -> A_1       | ●A_1 ◄4       | A_1 B b
  [
    ItemInfo { name: "plus_it", sym: NT(2), owner: 2 (A_1), is_vec: false, index: None },
    ItemInfo { name: "b", sym: NT(1), owner: 2, is_vec: false, index: None },
    ItemInfo { name: "b1", sym: T(1), owner: 2, is_vec: false, index: None }
  ],
  //  5: A_3 -> ε         | ◄5            | A_1 B b
  [
    ItemInfo { name: "plus_it", sym: NT(2), owner: 2, is_vec: false, index: None },
    ItemInfo { name: "b", sym: NT(1), owner: 2, is_vec: false, index: None },
    ItemInfo { name: "b1", sym: T(1), owner: 2, is_vec: false, index: None }
  ],
  //  6: A_4 -> A_2       | ●A_2 ◄6       | A_2 A_1 c
  [
    ItemInfo { name: "plus_it", sym: NT(3), owner: 3 (A_2), is_vec: false, index: None },
    ItemInfo { name: "plus", sym: NT(2), owner: 3, is_vec: false, index: None },
    ItemInfo { name: "c", sym: T(2), owner: 3, is_vec: false, index: None }
  ],
  //  7: A_4 -> ε         | ◄7            | A_2 A_1 c
  [
    ItemInfo { name: "plus", sym: NT(3), owner: 3, is_vec: false, index: None },
    ItemInfo { name: "plus1", sym: NT(2), owner: 3, is_vec: false, index: None },
    ItemInfo { name: "c", sym: T(2), owner: 3, is_vec: false, index: None }
  ]
]
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
            //  - A_3: child_left_fact (64)
            //  - A_4: child_left_fact (64)
            // parents:
            //  - A_1 -> A_2
            //  - A_2 -> A
            //  - A_3 -> A_1
            //  - A_4 -> A_2
            (RTS(30), 0, btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a! | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B b A_3   | ►A_3 b! ►B    |
                3 => symbols![],                        //  3: A_2 -> A_1 c A_4 | ►A_4 c! ►A_1  |
                4 => symbols![nt 2, nt 1, t 1],         //  4: A_3 -> A_1       | ●A_1 ◄4       | A_1 B b
                5 => symbols![nt 2, nt 1, t 1],         //  5: A_3 -> ε         | ◄5            | A_1 B b
                6 => symbols![nt 3, nt 2, t 2],         //  6: A_4 -> A_2       | ●A_2 ◄6       | A_2 A_1 c
                7 => symbols![nt 3, nt 2, t 2],         //  7: A_4 -> ε         | ◄7            | A_2 A_1 c
            ], All, btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_rts_30_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(30) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_2 d
        A { a: String, plus: SynA2, d: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// B -> b
        B { b: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<SynA1Item>);
    #[derive(Debug)]
    pub struct SynA1Item { b: SynB, b1: String }
    #[derive(Debug)]
    pub struct SynA2(Vec<SynA2Item>);
    #[derive(Debug)]
    pub struct SynA2Item { plus: SynA1, c: String }
    // User-defined: SynA, SynB
    #[derive(Debug)]
    pub struct SynA();
    #[derive(Debug)]
    pub struct SynB();

    #[derive(Debug)]
    enum SynValue { A(SynA), B(SynB), A1(SynA1), A2(SynA2) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_b(self) -> SynB {
            if let SynValue::B(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
        fn get_a2(self) -> SynA2 {
            if let SynValue::A2(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_b(&mut self) {}
        fn exit_b(&mut self, _ctx: CtxB) -> SynB;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        2 => self.init_a1(),                        // A_1
                        3 => self.init_a2(),                        // A_2
                        4 => {}                                     // A_3
                        5 => {}                                     // A_4
                        1 => self.listener.init_b(),                // B
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_2 d
                        4 |                                         // A_1 -> B b A_1
                        5 => self.exit_a1(),                        // A_1 -> B b
                        6 |                                         // A_2 -> A_1 c A_2
                        7 => self.exit_a2(),                        // A_2 -> A_1 c
                     /* 2 */                                        // A_1 -> B b | B b A_1 (never called)
                     /* 3 */                                        // A_2 -> A_1 c | A_1 c A_2 (never called)
                        1 => self.exit_b(),                         // B -> b
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let d = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a2();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, plus, d });
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let b1 = self.stack_t.pop().unwrap();
            let b = self.stack.pop().unwrap().get_b();
            let mut plus_it = self.stack.pop().unwrap().get_a1();
            plus_it.0.push(SynA1Item { b, b1 });
            self.stack.push(SynValue::A1(plus_it));
        }
        fn init_a2(&mut self) {
            let val = SynA2(Vec::new());
            self.stack.push(SynValue::A2(val));
        }
        fn exit_a2(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a1();
            let mut plus_it = self.stack.pop().unwrap().get_a2();
            plus_it.0.push(SynA2Item { plus, c });
            self.stack.push(SynValue::A2(plus_it));
        }
        fn exit_b(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let val = self.listener.exit_b(CtxB::B { b });
            self.stack.push(SynValue::B(val));
        }
    }

    // [wrapper source for rule RTS(30) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 14: rules RTS(30) #2, start 0:
/*
before, NT with value:
after,  NT with value: A_1, A_2
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
            //  - A_3: child_left_fact (64)
            //  - A_4: child_left_fact (64)
            // parents:
            //  - A_1 -> A_2
            //  - A_2 -> A
            //  - A_3 -> A_1
            //  - A_4 -> A_2
            (RTS(30), 0, btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a! | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B b A_3   | ►A_3 b! ►B    |
                3 => symbols![],                        //  3: A_2 -> A_1 c A_4 | ►A_4 c! ►A_1  |
                4 => symbols![nt 2, t 1],               //  4: A_3 -> A_1       | ●A_1 ◄4       | A_1 b
                5 => symbols![nt 2, t 1],               //  5: A_3 -> ε         | ◄5            | A_1 b
                6 => symbols![nt 3, nt 2, t 2],         //  6: A_4 -> A_2       | ●A_2 ◄6       | A_2 A_1 c
                7 => symbols![nt 3, nt 2, t 2],         //  7: A_4 -> ε         | ◄7            | A_2 A_1 c
            ], Set(symbols![t 0, t 1, t 2, t 3]), btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_rts_30_2 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(30) #2, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A } // A has no value: nothing returned from the top non-terminal
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_2 d
        A { a: String, plus: SynA2, d: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// B -> b
        B { b: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<String>);
    #[derive(Debug)]
    pub struct SynA2(Vec<SynA2Item>);
    #[derive(Debug)]
    pub struct SynA2Item { plus: SynA1, c: String }
    // Top non-terminal A has no value:
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A1(SynA1), A2(SynA2) }

    impl SynValue {
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
        fn get_a2(self) -> SynA2 {
            if let SynValue::A2(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) {}
        fn init_b(&mut self) {}
        fn exit_b(&mut self, _ctx: CtxB) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        2 => self.init_a1(),                        // A_1
                        3 => self.init_a2(),                        // A_2
                        4 => {}                                     // A_3
                        5 => {}                                     // A_4
                        1 => self.listener.init_b(),                // B
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_2 d
                        4 |                                         // A_1 -> B b A_1
                        5 => self.exit_a1(),                        // A_1 -> B b
                        6 |                                         // A_2 -> A_1 c A_2
                        7 => self.exit_a2(),                        // A_2 -> A_1 c
                     /* 2 */                                        // A_1 -> B b | B b A_1 (never called)
                     /* 3 */                                        // A_2 -> A_1 c | A_1 c A_2 (never called)
                        1 => self.exit_b(),                         // B -> b
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            self.listener.exit(Ctx::A);
        }
        fn exit_a(&mut self) {
            let d = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a2();
            let a = self.stack_t.pop().unwrap();
            self.listener.exit_a(CtxA::A { a, plus, d });
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let mut plus_it = self.stack.pop().unwrap().get_a1();
            plus_it.0.push(b);
            self.stack.push(SynValue::A1(plus_it));
        }
        fn init_a2(&mut self) {
            let val = SynA2(Vec::new());
            self.stack.push(SynValue::A2(val));
        }
        fn exit_a2(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a1();
            let mut plus_it = self.stack.pop().unwrap().get_a2();
            plus_it.0.push(SynA2Item { plus, c });
            self.stack.push(SynValue::A2(plus_it));
        }
        fn exit_b(&mut self) {
            let b = self.stack_t.pop().unwrap();
            self.listener.exit_b(CtxB::B { b });
        }
    }

    // [wrapper source for rule RTS(30) #2, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 15: rules RTS(24) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A, A_1
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(24), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![],                        //  1: A_1 -> b A_2 | ►A_2 b!       |
                2 => symbols![nt 1, t 1],               //  2: A_2 -> A_1   | ●A_1 ◄2       | A_1 b
                3 => symbols![nt 1, t 1],               //  3: A_2 -> ε     | ◄3            | A_1 b
            ], Default, btreemap![0 => vec![0]]),
*/
mod rules_rts_24_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(24) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::VarId;
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a A_1 c
        A { a: String, plus: SynAIter, c: String },
    }
    #[derive(Debug)]
    pub enum CtxAIter {
        /// A_1 -> b A_1
        A1_1 { plus_it: SynAIter, b: String },
        /// A_1 -> b
        A1_2 { plus_it: SynAIter, b: String },
    }

    // User-defined: SynA, SynAIter
    #[derive(Debug)]
    pub struct SynA();
    #[derive(Debug)]
    pub struct SynAIter();

    #[derive(Debug)]
    enum SynValue { A(SynA), AIter(SynAIter) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_a_iter(self) -> SynAIter {
            if let SynValue::AIter(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_a_iter(&mut self) -> SynAIter;
        fn exit_a_iter(&mut self, _ctx: CtxAIter) -> SynAIter;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => self.init_a_iter(),                    // A_1
                        2 => {}                                     // A_2
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_a(),                         // A -> a A_1 c
                        2 |                                         // A_1 -> b A_1
                        3 => self.exit_a_iter(),                    // A_1 -> b
                     /* 1 */                                        // A_1 -> b | b A_1 (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a_iter();
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A { a, plus, c });
            self.stack.push(SynValue::A(val));
        }
        fn init_a_iter(&mut self) {
            let val = self.listener.init_a_iter();
            self.stack.push(SynValue::AIter(val));
        }
        fn exit_a_iter(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let plus_it = self.stack.pop().unwrap().get_a_iter();
            let val = self.listener.exit_a_iter(CtxAIter::A1_1 { plus_it, b });
            self.stack.push(SynValue::AIter(val));
        }
    }

    // [wrapper source for rule RTS(24) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 16: rules PRS(28) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A
            // NT flags:
            //  - A: parent_left_fact (32)
            //  - A_1: parent_left_fact | child_left_fact (96)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (PRS(28), 0, btreemap![
                0 => symbols![],                        //  0: A -> a A_1   | ►A_1 a! |
                1 => symbols![t 4],                     //  1: A -> e       | ◄1 e!   | e
                2 => symbols![],                        //  2: A_1 -> b A_2 | ►A_2 b! |
                3 => symbols![t 0],                     //  3: A_1 -> ε     | ◄3      | a
                4 => symbols![t 0, t 1, t 2],           //  4: A_2 -> c     | ◄4 c!   | a b c
                5 => symbols![t 0, t 1, t 3],           //  5: A_2 -> d     | ◄5 d!   | a b d
                6 => symbols![t 0, t 1],                //  6: A_2 -> ε     | ◄6      | a b
            ], Default, btreemap![0 => vec![1, 3, 4, 5, 6]]),
*/
mod rules_prs_28_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(28) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> e
        A1 { e: String },
        /// A -> a
        A2 { a: String },
        /// A -> a b c
        A3 { a: String, b: String, c: String },
        /// A -> a b d
        A4 { a: String, b: String, d: String },
        /// A -> a b
        A5 { a: String, b: String },
    }

    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => {}                                     // A_1
                        2 => {}                                     // A_2
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        1 |                                         // A -> e
                        3 |                                         // A -> a
                        4 |                                         // A -> a b c
                        5 |                                         // A -> a b d
                        6 => self.exit_a(factor_id),                // A -> a b
                     /* 0 */                                        // A -> a | a b | a b c | a b d (never called)
                     /* 2 */                                        // A -> a b | a b c | a b d (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                1 => {
                    let e = self.stack_t.pop().unwrap();
                    CtxA::A1 { e }
                }
                3 => {
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A2 { a }
                }
                4 => {
                    let c = self.stack_t.pop().unwrap();
                    let b = self.stack_t.pop().unwrap();
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A3 { a, b, c }
                }
                5 => {
                    let d = self.stack_t.pop().unwrap();
                    let b = self.stack_t.pop().unwrap();
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A4 { a, b, d }
                }
                6 => {
                    let b = self.stack_t.pop().unwrap();
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A5 { a, b }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule PRS(28) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 17: rules PRS(31) #1, start 0:
/*
before, NT with value: E, F
after,  NT with value: E, F
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec (4)
            // parents:
            //  - E_1 -> E
            (PRS(31), 0, btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ►E_1 ◄0 ►F    | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!        | id
                2 => symbols![nt 0, t 1],               //  2: E_1 -> . id E_1 | ●E_1 ◄2 id! . | E id
                3 => symbols![],                        //  3: E_1 -> ε        | ◄3            |
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_prs_31_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(31) #1, start E]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { E { e: SynE } }
    #[derive(Debug)]
    pub enum CtxE {
        /// E -> F
        E1 { f: SynF },
        /// E -> E . id
        E2 { e: SynE, id: String },
        /// E -> ε (end of loop)
        E3 { e: SynE },
    }
    #[derive(Debug)]
    pub enum CtxF {
        /// F -> id
        F { id: String },
    }

    // User-defined: SynE, SynF
    #[derive(Debug)]
    pub struct SynE();
    #[derive(Debug)]
    pub struct SynF();

    #[derive(Debug)]
    enum SynValue { E(SynE), F(SynF) }

    impl SynValue {
        fn get_e(self) -> SynE {
            if let SynValue::E(val) = self { val } else { panic!() }
        }
        fn get_f(self) -> SynF {
            if let SynValue::F(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_e(&mut self) {}
        fn exit_e(&mut self, _ctx: CtxE) -> SynE;
        fn init_f(&mut self) {}
        fn exit_f(&mut self, _ctx: CtxF) -> SynF;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_e(),                // E
                        2 => {}                                     // E_1
                        1 => self.listener.init_f(),                // F
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.init_e(),                         // E -> F
                        2 |                                         // E -> E . id
                        3 => self.exit_e1(factor_id),               // E -> ε (end of loop)
                        1 => self.exit_f(),                         // F -> id
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let e = self.stack.pop().unwrap().get_e();
            self.listener.exit(Ctx::E { e });
        }
        fn init_e(&mut self) {
            let f = self.stack.pop().unwrap().get_f();
            let val = self.listener.exit_e(CtxE::E1 { f });
            self.stack.push(SynValue::E(val));
        }
        fn exit_e1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                2 => {
                    let id = self.stack_t.pop().unwrap();
                    let e = self.stack.pop().unwrap().get_e();
                    CtxE::E2 { e, id }
                }
                3 => {
                    let e = self.stack.pop().unwrap().get_e();
                    CtxE::E3 { e }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_e1")
            };
            let val = self.listener.exit_e(ctx);
            self.stack.push(SynValue::E(val));
        }
        fn exit_f(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_f(CtxF::F { id });
            self.stack.push(SynValue::F(val));
        }
    }

    // [wrapper source for rule PRS(31) #1, start E]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 18: rules PRS(36) #1, start 0:
/*
before, NT with value: E, F
after,  NT with value: E, F
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec (4)
            // parents:
            //  - E_1 -> E
            (PRS(36), 0, btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ►E_1 ◄0 ►F    | F
                1 => symbols![t 2],                     //  1: E -> num E_1    | ►E_1 ◄1 num!  | num
                2 => symbols![t 1],                     //  2: F -> id         | ◄2 id!        | id
                3 => symbols![nt 0, t 1],               //  3: E_1 -> . id E_1 | ●E_1 ◄3 id! . | E id
                4 => symbols![],                        //  4: E_1 -> ε        | ◄4            |
            ], Default, btreemap![0 => vec![0, 1], 1 => vec![2]]),
*/
mod rules_prs_36_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(36) #1, start E]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { E { e: SynE } }
    #[derive(Debug)]
    pub enum CtxE {
        /// E -> F
        E1 { f: SynF },
        /// E -> num
        E2 { num: String },
        /// E -> E . id
        E3 { e: SynE, id: String },
        /// E -> ε (end of loop)
        E4 { e: SynE },
    }
    #[derive(Debug)]
    pub enum CtxF {
        /// F -> id
        F { id: String },
    }

    // User-defined: SynE, SynF
    #[derive(Debug)]
    pub struct SynE();
    #[derive(Debug)]
    pub struct SynF();

    #[derive(Debug)]
    enum SynValue { E(SynE), F(SynF) }

    impl SynValue {
        fn get_e(self) -> SynE {
            if let SynValue::E(val) = self { val } else { panic!() }
        }
        fn get_f(self) -> SynF {
            if let SynValue::F(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_e(&mut self) {}
        fn exit_e(&mut self, _ctx: CtxE) -> SynE;
        fn init_f(&mut self) {}
        fn exit_f(&mut self, _ctx: CtxF) -> SynF;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_e(),                // E
                        2 => {}                                     // E_1
                        1 => self.listener.init_f(),                // F
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 |                                         // E -> F
                        1 => self.init_e(factor_id),                // E -> num
                        3 |                                         // E -> E . id
                        4 => self.exit_e1(factor_id),               // E -> ε (end of loop)
                        2 => self.exit_f(),                         // F -> id
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let e = self.stack.pop().unwrap().get_e();
            self.listener.exit(Ctx::E { e });
        }
        fn init_e(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                0 => {
                    let f = self.stack.pop().unwrap().get_f();
                    CtxE::E1 { f }
                }
                1 => {
                    let num = self.stack_t.pop().unwrap();
                    CtxE::E2 { num }
                }
                _ => panic!("unexpected factor id {factor_id} in fn init_e")
            };
            let val = self.listener.exit_e(ctx);
            self.stack.push(SynValue::E(val));
        }
        fn exit_e1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                3 => {
                    let id = self.stack_t.pop().unwrap();
                    let e = self.stack.pop().unwrap().get_e();
                    CtxE::E3 { e, id }
                }
                4 => {
                    let e = self.stack.pop().unwrap().get_e();
                    CtxE::E4 { e }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_e1")
            };
            let val = self.listener.exit_e(ctx);
            self.stack.push(SynValue::E(val));
        }
        fn exit_f(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_f(CtxF::F { id });
            self.stack.push(SynValue::F(val));
        }
    }

    // [wrapper source for rule PRS(36) #1, start E]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 19: rules PRS(33) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec (4)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (PRS(33), 0, btreemap![
                0 => symbols![],                        //  0: A -> b A_2   | ►A_2 b!    |
                1 => symbols![nt 0, t 0],               //  1: A_1 -> a A_1 | ●A_1 ◄1 a! | A a
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2         |
                3 => symbols![t 1, t 2],                //  3: A_2 -> c A_1 | ►A_1 ◄3 c! | b c
                4 => symbols![t 1, t 3],                //  4: A_2 -> d A_1 | ►A_1 ◄4 d! | b d
            ], Default, btreemap![0 => vec![3, 4]]),
*/
mod rules_prs_33_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(33) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> A a
        A1 { a: SynA, a1: String },
        /// A -> ε (end of loop)
        A2 { a: SynA },
        /// A -> b c
        A3 { b: String, c: String },
        /// A -> b d
        A4 { b: String, d: String },
    }

    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => {}                                     // A_1
                        2 => {}                                     // A_2
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        3 |                                         // A -> b c
                        4 => self.init_a(factor_id),                // A -> b d
                        1 |                                         // A -> A a
                        2 => self.exit_a1(factor_id),               // A -> ε (end of loop)
                     /* 0 */                                        // A -> b c | b d (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn init_a(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                3 => {
                    let c = self.stack_t.pop().unwrap();
                    let b = self.stack_t.pop().unwrap();
                    CtxA::A3 { b, c }
                }
                4 => {
                    let d = self.stack_t.pop().unwrap();
                    let b = self.stack_t.pop().unwrap();
                    CtxA::A4 { b, d }
                }
                _ => panic!("unexpected factor id {factor_id} in fn init_a")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
        fn exit_a1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                1 => {
                    let a1 = self.stack_t.pop().unwrap();
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A1 { a, a1 }
                }
                2 => {
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A2 { a }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a1")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule PRS(33) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 20: rules PRS(38) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec (4)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (PRS(38), 0, btreemap![
                0 => symbols![],                        //  0: A -> b A_2   | ►A_2 b!    |
                1 => symbols![nt 0, t 0],               //  1: A_1 -> a A_1 | ●A_1 ◄1 a! | A a
                2 => symbols![nt 0, t 1],               //  2: A_1 -> b A_1 | ●A_1 ◄2 b! | A b
                3 => symbols![],                        //  3: A_1 -> ε     | ◄3         |
                4 => symbols![t 1, t 2],                //  4: A_2 -> c A_1 | ►A_1 ◄4 c! | b c
                5 => symbols![t 1, t 3],                //  5: A_2 -> d A_1 | ►A_1 ◄5 d! | b d
            ], Default, btreemap![0 => vec![4, 5]]),
*/
mod rules_prs_38_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(38) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> A a
        A1 { a: SynA, a1: String },
        /// A -> A b
        A2 { a: SynA, b: String },
        /// A -> ε (end of loop)
        A3 { a: SynA },
        /// A -> b c
        A4 { b: String, c: String },
        /// A -> b d
        A5 { b: String, d: String },
    }

    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => {}                                     // A_1
                        2 => {}                                     // A_2
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        4 |                                         // A -> b c
                        5 => self.init_a(factor_id),                // A -> b d
                        1 |                                         // A -> A a
                        2 |                                         // A -> A b
                        3 => self.exit_a1(factor_id),               // A -> ε (end of loop)
                     /* 0 */                                        // A -> b c | b d (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn init_a(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                4 => {
                    let c = self.stack_t.pop().unwrap();
                    let b = self.stack_t.pop().unwrap();
                    CtxA::A4 { b, c }
                }
                5 => {
                    let d = self.stack_t.pop().unwrap();
                    let b = self.stack_t.pop().unwrap();
                    CtxA::A5 { b, d }
                }
                _ => panic!("unexpected factor id {factor_id} in fn init_a")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
        fn exit_a1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                1 => {
                    let a1 = self.stack_t.pop().unwrap();
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A1 { a, a1 }
                }
                2 => {
                    let b = self.stack_t.pop().unwrap();
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A2 { a, b }
                }
                3 => {
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A3 { a }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a1")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule PRS(38) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 21: rules PRS(39) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec | parent_left_fact (36)
            //  - A_2: child_left_fact (64)
            //  - A_3: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            //  - A_3 -> A_1
            (PRS(39), 0, btreemap![                     /// A -> A a b | A a c | b c | b d
                0 => symbols![],                        //  0: A -> b A_2   | ►A_2 b!    |
                1 => symbols![],                        //  1: A_1 -> a A_3 | ►A_3 a!    |
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2         |
                3 => symbols![t 1, t 2],                //  3: A_2 -> c A_1 | ►A_1 ◄3 c! | b c
                4 => symbols![t 1, t 3],                //  4: A_2 -> d A_1 | ►A_1 ◄4 d! | b d
                5 => symbols![nt 0, t 0, t 1],          //  5: A_3 -> b A_1 | ●A_1 ◄5 b! | A a b
                6 => symbols![nt 0, t 0, t 2],          //  6: A_3 -> c A_1 | ●A_1 ◄6 c! | A a c
            ], Default, btreemap![0 => vec![3, 4]]),
*/
mod rules_prs_39_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(39) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> ε (end of loop)
        A1 { a: SynA },
        /// A -> b c
        A2 { b: String, c: String },
        /// A -> b d
        A3 { b: String, d: String },
        /// A -> A a b
        A4 { a: SynA, a1: String, b: String },
        /// A -> A a c
        A5 { a: SynA, a1: String, c: String },
    }

    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => {}                                     // A_1
                        2 => {}                                     // A_2
                        3 => {}                                     // A_3
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        3 |                                         // A -> b c
                        4 => self.init_a(factor_id),                // A -> b d
                        2 |                                         // A -> ε (end of loop)
                        5 |                                         // A -> A a b
                        6 => self.exit_a1(factor_id),               // A -> A a c
                     /* 0 */                                        // A -> b c | b d (never called)
                     /* 1 */                                        // A -> A a b | A a c (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn init_a(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                3 => {
                    let c = self.stack_t.pop().unwrap();
                    let b = self.stack_t.pop().unwrap();
                    CtxA::A2 { b, c }
                }
                4 => {
                    let d = self.stack_t.pop().unwrap();
                    let b = self.stack_t.pop().unwrap();
                    CtxA::A3 { b, d }
                }
                _ => panic!("unexpected factor id {factor_id} in fn init_a")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
        fn exit_a1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                2 => {
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A1 { a }
                }
                5 => {
                    let b = self.stack_t.pop().unwrap();
                    let a1 = self.stack_t.pop().unwrap();
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A4 { a, a1, b }
                }
                6 => {
                    let c = self.stack_t.pop().unwrap();
                    let a1 = self.stack_t.pop().unwrap();
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A5 { a, a1, c }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a1")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule PRS(39) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 21: rules PRS(32) #1, start 0:
/*
before, NT with value: E, F
after,  NT with value: E, F
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec | parent_left_fact (36)
            //  - E_2: child_left_fact (64)
            // parents:
            //  - E_1 -> E
            //  - E_2 -> E_1
            (PRS(32), 0, btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ►E_1 ◄0 ►F  | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!      | id
                2 => symbols![],                        //  2: E_1 -> . id E_2 | ►E_2 id! .  |
                3 => symbols![],                        //  3: E_1 -> ε        | ◄3          |
                4 => symbols![nt 0, t 1],               //  4: E_2 -> ( ) E_1  | ●E_1 ◄4 ) ( | E id
                5 => symbols![nt 0, t 1],               //  5: E_2 -> E_1      | ●E_1 ◄5     | E id
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),
*/
mod rules_prs_32_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(32) #1, start E]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { E { e: SynE } }
    #[derive(Debug)]
    pub enum CtxE {
        /// E -> F
        E1 { f: SynF },
        /// E -> ε (end of loop)
        E2 { e: SynE },
        /// E -> E . id ( )
        E3 { e: SynE, id: String },
        /// E -> E . id
        E4 { e: SynE, id: String },
    }
    #[derive(Debug)]
    pub enum CtxF {
        /// F -> id
        F { id: String },
    }

    // User-defined: SynE, SynF
    #[derive(Debug)]
    pub struct SynE();
    #[derive(Debug)]
    pub struct SynF();

    #[derive(Debug)]
    enum SynValue { E(SynE), F(SynF) }

    impl SynValue {
        fn get_e(self) -> SynE {
            if let SynValue::E(val) = self { val } else { panic!() }
        }
        fn get_f(self) -> SynF {
            if let SynValue::F(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_e(&mut self) {}
        fn exit_e(&mut self, _ctx: CtxE) -> SynE;
        fn init_f(&mut self) {}
        fn exit_f(&mut self, _ctx: CtxF) -> SynF;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_e(),                // E
                        2 => {}                                     // E_1
                        3 => {}                                     // E_2
                        1 => self.listener.init_f(),                // F
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.init_e(),                         // E -> F
                        3 |                                         // E -> ε (end of loop)
                        4 |                                         // E -> E . id ( )
                        5 => self.exit_e1(factor_id),               // E -> E . id
                     /* 2 */                                        // E -> E . id ( ) | E . id (never called)
                        1 => self.exit_f(),                         // F -> id
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let e = self.stack.pop().unwrap().get_e();
            self.listener.exit(Ctx::E { e });
        }
        fn init_e(&mut self) {
            let f = self.stack.pop().unwrap().get_f();
            let val = self.listener.exit_e(CtxE::E1 { f });
            self.stack.push(SynValue::E(val));
        }
        fn exit_e1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                3 => {
                    let e = self.stack.pop().unwrap().get_e();
                    CtxE::E2 { e }
                }
                4 => {
                    let id = self.stack_t.pop().unwrap();
                    let e = self.stack.pop().unwrap().get_e();
                    CtxE::E3 { e, id }
                }
                5 => {
                    let id = self.stack_t.pop().unwrap();
                    let e = self.stack.pop().unwrap().get_e();
                    CtxE::E4 { e, id }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_e1")
            };
            let val = self.listener.exit_e(ctx);
            self.stack.push(SynValue::E(val));
        }
        fn exit_f(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_f(CtxF::F { id });
            self.stack.push(SynValue::F(val));
        }
    }

    // [wrapper source for rule PRS(32) #1, start E]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 22: rules PRS(20) #1, start 0:
/*
before, NT with value: STRUCT, LIST
after,  NT with value: STRUCT, LIST
            // NT flags:
            //  - LIST: right_rec (2)
            // parents:
            //  - (nothing)
            (PRS(20), 0, btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![t 5, t 5, nt 1],          //  1: LIST -> id : id ; LIST     | ◄1 ►LIST ; id! : id!  | id id LIST
                2 => symbols![],                        //  2: LIST -> }                  | ◄2 }                  |
            ], Default, btreemap![0 => vec![0], 1 => vec![1, 2]]),
*/
mod rules_prs_20_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(20) #1, start STRUCT]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { Struct { struct1: SynStruct } }
    #[derive(Debug)]
    pub enum CtxStruct {
        /// STRUCT -> struct id { LIST
        Struct { id: String, list: SynList },
    }
    #[derive(Debug)]
    pub enum CtxList {
        /// LIST -> id : id ; LIST
        List1 { id: [String; 2], list: SynList },
        /// LIST -> }
        List2,
    }

    // User-defined: SynStruct, SynList
    #[derive(Debug)]
    pub struct SynStruct();
    #[derive(Debug)]
    pub struct SynList();

    #[derive(Debug)]
    enum SynValue { Struct(SynStruct), List(SynList) }

    impl SynValue {
        fn get_struct1(self) -> SynStruct {
            if let SynValue::Struct(val) = self { val } else { panic!() }
        }
        fn get_list(self) -> SynList {
            if let SynValue::List(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_struct1(&mut self) {}
        fn exit_struct1(&mut self, _ctx: CtxStruct) -> SynStruct;
        fn init_list(&mut self) {}
        fn exit_list(&mut self, _ctx: CtxList) -> SynList;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_struct1(),          // STRUCT
                        1 => self.listener.init_list(),             // LIST
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_struct1(),                   // STRUCT -> struct id { LIST
                        1 |                                         // LIST -> id : id ; LIST
                        2 => self.exit_list(factor_id),             // LIST -> }
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let struct1 = self.stack.pop().unwrap().get_struct1();
            self.listener.exit(Ctx::Struct { struct1 });
        }
        fn exit_struct1(&mut self) {
            let list = self.stack.pop().unwrap().get_list();
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_struct1(CtxStruct::Struct { id, list });
            self.stack.push(SynValue::Struct(val));
        }
        fn exit_list(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                1 => {
                    let list = self.stack.pop().unwrap().get_list();
                    let id_2 = self.stack_t.pop().unwrap();
                    let id_1 = self.stack_t.pop().unwrap();
                    CtxList::List1 { id: [id_1, id_2], list }
                }
                2 => {
                    CtxList::List2
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_list")
            };
            let val = self.listener.exit_list(ctx);
            self.stack.push(SynValue::List(val));
        }
    }

    // [wrapper source for rule PRS(20) #1, start STRUCT]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 23: rules PRS(20) #2, start 0:
/*
before, NT with value: STRUCT
after,  NT with value: STRUCT
            // NT flags:
            //  - LIST: right_rec (2)
            // parents:
            //  - (nothing)
            (PRS(20), 0, btreemap![
                0 => symbols![t 5],                     //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id
                1 => symbols![t 5, t 5],                //  1: LIST -> id : id ; LIST     | ◄1 ►LIST ; id! : id!  | id id
                2 => symbols![],                        //  2: LIST -> }                  | ◄2 }                  |
            ], Set(symbols![nt 0, t 5]), btreemap![0 => vec![0], 1 => vec![1, 2]]),
*/
mod rules_prs_20_2 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(20) #2, start STRUCT]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { Struct { struct1: SynStruct } }
    #[derive(Debug)]
    pub enum CtxStruct {
        /// STRUCT -> struct id { LIST
        Struct { id: String },
    }
    #[derive(Debug)]
    pub enum CtxList {
        /// LIST -> id : id ; LIST
        List1 { id: [String; 2] },
        /// LIST -> }
        List2,
    }

    // User-defined: SynStruct
    #[derive(Debug)]
    pub struct SynStruct();

    #[derive(Debug)]
    enum SynValue { Struct(SynStruct) }

    impl SynValue {
        fn get_struct1(self) -> SynStruct {
            let SynValue::Struct(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_struct1(&mut self) {}
        fn exit_struct1(&mut self, _ctx: CtxStruct) -> SynStruct;
        fn init_list(&mut self) {}
        fn exit_list(&mut self, _ctx: CtxList) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_struct1(),          // STRUCT
                        1 => self.listener.init_list(),             // LIST
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_struct1(),                   // STRUCT -> struct id { LIST
                        1 |                                         // LIST -> id : id ; LIST
                        2 => self.exit_list(factor_id),             // LIST -> }
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let struct1 = self.stack.pop().unwrap().get_struct1();
            self.listener.exit(Ctx::Struct { struct1 });
        }
        fn exit_struct1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_struct1(CtxStruct::Struct { id });
            self.stack.push(SynValue::Struct(val));
        }
        fn exit_list(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                1 => {
                    let id_2 = self.stack_t.pop().unwrap();
                    let id_1 = self.stack_t.pop().unwrap();
                    CtxList::List1 { id: [id_1, id_2] }
                }
                2 => {
                    CtxList::List2
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_list")
            };
            self.listener.exit_list(ctx);
        }
    }

    // [wrapper source for rule PRS(20) #2, start STRUCT]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 24: rules PRS(37) #1, start 0:
/*
before, NT with value: STRUCT, LIST
after,  NT with value: STRUCT, LIST
            // NT flags:
            //  - LIST: right_rec | parent_left_fact (34)
            //  - LIST_1: child_left_fact (64)
            // parents:
            //  - LIST_1 -> LIST
            (PRS(37), 0, btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![],                        //  1: LIST -> }                  | ◄1 }                  |
                2 => symbols![],                        //  2: LIST -> id LIST_1          | ►LIST_1 id!           |
                3 => symbols![t 5, t 5],                //  3: LIST_1 -> : id ; LIST      | ●LIST ◄3 ; id! :      | id id
                4 => symbols![t 5],                     //  4: LIST_1 -> ; LIST           | ●LIST ◄4 ;            | id
            ], Default, btreemap![0 => vec![0], 1 => vec![1, 3, 4]]),
*/
mod rules_prs_37_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(37) #1, start STRUCT]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { Struct { struct1: SynStruct } }
    #[derive(Debug)]
    pub enum CtxStruct {
        /// STRUCT -> struct id { LIST
        Struct { id: String, list: SynList },
    }
    #[derive(Debug)]
    pub enum CtxList {
        /// LIST -> }
        List1,
        /// LIST -> id : id ; LIST
        List2 { id: [String; 2] },
        /// LIST -> id ; LIST
        List3 { id: String },
    }

    // User-defined: SynStruct, SynList
    #[derive(Debug)]
    pub struct SynStruct();
    #[derive(Debug)]
    pub struct SynList();

    #[derive(Debug)]
    enum SynValue { Struct(SynStruct), List(SynList) }

    impl SynValue {
        fn get_struct1(self) -> SynStruct {
            if let SynValue::Struct(val) = self { val } else { panic!() }
        }
        fn get_list(self) -> SynList {
            if let SynValue::List(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_struct1(&mut self) {}
        fn exit_struct1(&mut self, _ctx: CtxStruct) -> SynStruct;
        fn init_list(&mut self) {}
        fn exit_list(&mut self, _ctx: CtxList) -> SynList;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_struct1(),          // STRUCT
                        1 => self.listener.init_list(),             // LIST
                        2 => {}                                     // LIST_1
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_struct1(),                   // STRUCT -> struct id { LIST
                        1 |                                         // LIST -> }
                        3 |                                         // LIST -> id : id ; LIST
                        4 => self.exit_list(factor_id),             // LIST -> id ; LIST
                     /* 2 */                                        // LIST -> id : id ; LIST | id ; LIST (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let struct1 = self.stack.pop().unwrap().get_struct1();
            self.listener.exit(Ctx::Struct { struct1 });
        }
        fn exit_struct1(&mut self) {
            let list = self.stack.pop().unwrap().get_list();
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_struct1(CtxStruct::Struct { id, list });
            self.stack.push(SynValue::Struct(val));
        }
        fn exit_list(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                1 => {
                    CtxList::List1
                }
                3 => {
                    let id_2 = self.stack_t.pop().unwrap();
                    let id_1 = self.stack_t.pop().unwrap();
                    CtxList::List2 { id: [id_1, id_2] }
                }
                4 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxList::List3 { id }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_list")
            };
            let val = self.listener.exit_list(ctx);
            self.stack.push(SynValue::List(val));
        }
    }

    // [wrapper source for rule PRS(37) #1, start STRUCT]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 25: rules PRS(30) #1, start 0:
/*
before, NT with value: STRUCT, LIST
after,  NT with value: STRUCT, LIST
            // NT flags:
            //  - LIST: right_rec | L-form (130)
            // parents:
            //  - (nothing)
            (PRS(30), 0, btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![nt 1, t 5, t 5],          //  1: LIST -> id : id ; LIST     | ●LIST ◄1 ; id! : id!  | LIST id id
                2 => symbols![nt 1],                    //  2: LIST -> }                  | ◄2 }                  | LIST
            ], Default, btreemap![0 => vec![0], 1 => vec![1, 2]]),
*/
mod rules_prs_30_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(30) #1, start STRUCT]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { Struct { struct1: SynStruct } }
    #[derive(Debug)]
    pub enum CtxStruct {
        /// STRUCT -> struct id { LIST
        Struct { id: String, list: SynList },
    }
    #[derive(Debug)]
    pub enum CtxList {
        /// LIST -> id : id ; LIST
        List1 { list: SynList, id: [String; 2] },
        /// LIST -> }
        List2 { list: SynList },
    }

    // User-defined: SynStruct, SynList
    #[derive(Debug)]
    pub struct SynStruct();
    #[derive(Debug)]
    pub struct SynList();

    #[derive(Debug)]
    enum SynValue { Struct(SynStruct), List(SynList) }

    impl SynValue {
        fn get_struct1(self) -> SynStruct {
            if let SynValue::Struct(val) = self { val } else { panic!() }
        }
        fn get_list(self) -> SynList {
            if let SynValue::List(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_struct1(&mut self) {}
        fn exit_struct1(&mut self, _ctx: CtxStruct) -> SynStruct;
        fn init_list(&mut self) -> SynList;
        fn exit_list(&mut self, _ctx: CtxList) -> SynList;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_struct1(),          // STRUCT
                        1 => self.init_list(),                      // LIST
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_struct1(),                   // STRUCT -> struct id { LIST
                        1 |                                         // LIST -> id : id ; LIST
                        2 => self.exit_list(factor_id),             // LIST -> }
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let struct1 = self.stack.pop().unwrap().get_struct1();
            self.listener.exit(Ctx::Struct { struct1 });
        }
        fn exit_struct1(&mut self) {
            let list = self.stack.pop().unwrap().get_list();
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_struct1(CtxStruct::Struct { id, list });
            self.stack.push(SynValue::Struct(val));
        }
        fn init_list(&mut self) {
            let val = self.listener.init_list();
            self.stack.push(SynValue::List(val));
        }
        fn exit_list(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                1 => {
                    let id_2 = self.stack_t.pop().unwrap();
                    let id_1 = self.stack_t.pop().unwrap();
                    let list = self.stack.pop().unwrap().get_list();
                    CtxList::List1 { list, id: [id_1, id_2] }
                }
                2 => {
                    let list = self.stack.pop().unwrap().get_list();
                    CtxList::List2 { list }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_list")
            };
            let val = self.listener.exit_list(ctx);
            self.stack.push(SynValue::List(val));
        }
    }

    // [wrapper source for rule PRS(30) #1, start STRUCT]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 26: rules RTS(26) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A, A_1
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* (2560)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_left_rec (4)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (RTS(26), 0, btreemap![
                0 => symbols![t 0],                     //  0: A -> a A_2       | ►A_2 ◄0 a!      | a
                1 => symbols![nt 1, t 2],               //  1: A_1 -> c A_1     | ●A_1 ◄1 c!      | A_1 c
                2 => symbols![],                        //  2: A_1 -> ε         | ◄2              |
                3 => symbols![nt 0, nt 1, t 1],         //  3: A_2 -> A_1 b A_2 | ●A_2 ◄3 b! ►A_1 | A A_1 b
                4 => symbols![],                        //  4: A_2 -> ε         | ◄4              |
            ], Default, btreemap![0 => vec![0]]),
*/
mod rules_rts_26_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(26) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a
        A1 { a: String },
        /// A -> A A_1 b
        A2 { a: SynA, star: SynA1, b: String },
        /// A -> ε (end of loop)
        A3 { a: SynA },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<String>);
    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA), A1(SynA1) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => self.init_a1(),                        // A_1
                        2 => {}                                     // A_2
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.init_a(),                         // A -> a
                        1 => self.exit_a1(),                        // A_1 -> c A_1
                        2 => {}                                     // A_1 -> ε
                        3 |                                         // A -> A A_1 b
                        4 => self.exit_a2(factor_id),               // A -> ε (end of loop)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn init_a(&mut self) {
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A1 { a });
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let mut star_it = self.stack.pop().unwrap().get_a1();
            star_it.0.push(c);
            self.stack.push(SynValue::A1(star_it));
        }
        fn exit_a2(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                3 => {
                    let b = self.stack_t.pop().unwrap();
                    let star = self.stack.pop().unwrap().get_a1();
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A2 { a, star, b }
                }
                4 => {
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A3 { a }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a2")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule RTS(26) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 27: rules RTS(16) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A, A_1
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* | plus (6656)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_rec (4)
            //  - A_3: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            //  - A_3 -> A_1
            (RTS(16), 0, btreemap![
                0 => symbols![t 0],                     //  0: A -> a A_2       | ►A_2 ◄0 a!      | a
                1 => symbols![],                        //  1: A_1 -> c A_3     | ►A_3 c!         |
                2 => symbols![nt 0, nt 1, t 1],         //  2: A_2 -> A_1 b A_2 | ●A_2 ◄2 b! ►A_1 | A A_1 b
                3 => symbols![],                        //  3: A_2 -> ε         | ◄3              |
                4 => symbols![nt 1, t 2],               //  4: A_3 -> A_1       | ●A_1 ◄4         | A_1 c
                5 => symbols![nt 1, t 2],               //  5: A_3 -> ε         | ◄5              | A_1 c
            ], Default, btreemap![0 => vec![0]]),
*/
mod rules_rts_16_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(16) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a
        A1 { a: String },
        /// A -> A A_1 b
        A2 { a: SynA, plus: SynA1, b: String },
        /// A -> ε (end of loop)
        A3 { a: SynA },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<String>);
    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA), A1(SynA1) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => self.init_a1(),                        // A_1
                        2 => {}                                     // A_2
                        3 => {}                                     // A_3
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.init_a(),                         // A -> a
                        4 |                                         // A_1 -> c A_1
                        5 => self.exit_a1(),                        // A_1 -> c
                        2 |                                         // A -> A A_1 b
                        3 => self.exit_a2(factor_id),               // A -> ε (end of loop)
                     /* 1 */                                        // A_1 -> c | c A_1 (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn init_a(&mut self) {
            let a = self.stack_t.pop().unwrap();
            let val = self.listener.exit_a(CtxA::A1 { a });
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let mut plus_it = self.stack.pop().unwrap().get_a1();
            plus_it.0.push(c);
            self.stack.push(SynValue::A1(plus_it));
        }
        fn exit_a2(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                2 => {
                    let b = self.stack_t.pop().unwrap();
                    let plus = self.stack.pop().unwrap().get_a1();
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A2 { a, plus, b }
                }
                3 => {
                    let a = self.stack.pop().unwrap().get_a();
                    CtxA::A3 { a }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a2")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule RTS(16) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 28: rules PRS(35) #1, start 0:
/*
before, NT with value: A
after,  NT with value: A
            // NT flags:
            //  - A: parent_left_fact (32)
            //  - A_1: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            (PRS(35), 0, btreemap![
                0 => symbols![],                        //  0: A -> a A_1 | ►A_1 a!  |
                1 => symbols![t 0, t 1, t 1],           //  1: A_1 -> b b | ◄1 b! b! | a b b
                2 => symbols![t 0, t 2, t 2],           //  2: A_1 -> c c | ◄2 c! c! | a c c
                3 => symbols![t 0],                     //  3: A_1 -> ε   | ◄3       | a
            ], Default, btreemap![0 => vec![1, 2, 3]]),
*/
mod rules_prs_35_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule PRS(35) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> a b b
        A1 { a: String, b: [String; 2] },
        /// A -> a c c
        A2 { a: String, c: [String; 2] },
        /// A -> a
        A3 { a: String },
    }

    // User-defined: SynA
    #[derive(Debug)]
    pub struct SynA();

    #[derive(Debug)]
    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        1 => {}                                     // A_1
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        1 |                                         // A -> a b b
                        2 |                                         // A -> a c c
                        3 => self.exit_a(factor_id),                // A -> a
                     /* 0 */                                        // A -> a | a b b | a c c (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                1 => {
                    let b_2 = self.stack_t.pop().unwrap();
                    let b_1 = self.stack_t.pop().unwrap();
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A1 { a, b: [b_1, b_2] }
                }
                2 => {
                    let c_2 = self.stack_t.pop().unwrap();
                    let c_1 = self.stack_t.pop().unwrap();
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A2 { a, c: [c_1, c_2] }
                }
                3 => {
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A3 { a }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
    }

    // [wrapper source for rule PRS(35) #1, start A]
    // ------------------------------------------------------------

}

// ================================================================================
// Test 29: rules RTS(33) #1, start 0:
/*
before, NT with value: A, B
after,  NT with value: A, B, A_1
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(33), 0, btreemap![
                0 => symbols![nt 2, t 1],               //  0: A -> A_1 b     | ◄0 b! ►A_1    | A_1 b
                1 => symbols![t 0],                     //  1: A -> a         | ◄1 a!         | a
                2 => symbols![t 1],                     //  2: B -> b         | ◄2 b!         | b
                3 => symbols![nt 2, nt 1, t 2],         //  3: A_1 -> B c A_1 | ●A_1 ◄3 c! ►B | A_1 B c
                4 => symbols![],                        //  4: A_1 -> ε       | ◄4            |
            ], All, btreemap![0 => vec![0, 1], 1 => vec![2]]),
*/
mod rules_rts_33_1 {
    // ------------------------------------------------------------
    // [wrapper source for rule RTS(33) #1, start A]

    use rlexer::CollectJoin;
    use rlexer::grammar::{FactorId, VarId};
    use rlexer::parser::{Call, Listener};

    #[derive(Debug)]
    pub enum Ctx { A { a: SynA } }
    #[derive(Debug)]
    pub enum CtxA {
        /// A -> A_1 b
        A1 { star: SynA1, b: String },
        /// A -> a
        A2 { a: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// B -> b
        B { b: String },
    }

    #[derive(Debug)]
    pub struct SynA1(Vec<SynA1Item>);
    #[derive(Debug)]
    pub struct SynA1Item { b: SynB, c: String }
    // User-defined: SynA, SynB
    #[derive(Debug)]
    pub struct SynA();
    #[derive(Debug)]
    pub struct SynB();

    #[derive(Debug)]
    enum SynValue { A(SynA), B(SynB), A1(SynA1) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_b(self) -> SynB {
            if let SynValue::B(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let SynValue::A1(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
        fn init_a(&mut self) {}
        fn exit_a(&mut self, _ctx: CtxA) -> SynA;
        fn init_b(&mut self) {}
        fn exit_b(&mut self, _ctx: CtxB) -> SynB;
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: TestListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_a(),                // A
                        2 => self.init_a1(),                        // A_1
                        1 => self.listener.init_b(),                // B
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 |                                         // A -> A_1 b
                        1 => self.exit_a(factor_id),                // A -> a
                        3 => self.exit_a1(),                        // A_1 -> B c A_1
                        4 => {}                                     // A_1 -> ε
                        2 => self.exit_b(),                         // B -> b
                        _ => panic!("unexpected exit factor id: {factor_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }
    }

    impl<T: TestListener> ListenerWrapper<T> {
        fn exit(&mut self) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
        fn exit_a(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                0 => {
                    let b = self.stack_t.pop().unwrap();
                    let star = self.stack.pop().unwrap().get_a1();
                    CtxA::A1 { star, b }
                }
                1 => {
                    let a = self.stack_t.pop().unwrap();
                    CtxA::A2 { a }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_a")
            };
            let val = self.listener.exit_a(ctx);
            self.stack.push(SynValue::A(val));
        }
        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(SynValue::A1(val));
        }
        fn exit_a1(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let b = self.stack.pop().unwrap().get_b();
            let mut star_it = self.stack.pop().unwrap().get_a1();
            star_it.0.push(SynA1Item { b, c });
            self.stack.push(SynValue::A1(star_it));
        }
        fn exit_b(&mut self) {
            let b = self.stack_t.pop().unwrap();
            let val = self.listener.exit_b(CtxB::B { b });
            self.stack.push(SynValue::B(val));
        }
    }

    // [wrapper source for rule RTS(33) #1, start A]
    // ------------------------------------------------------------
}
