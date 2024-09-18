================================================================================
Test 0: rules PRS(34), start 0:
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
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 0: PRS(34), start S]

    pub enum Ctx { S { s: SynS } }
    pub enum CtxS {
        S1 { id: String, val: SynVal },
        S2 { val: SynVal },
    }
    pub enum CtxVal {
        Val1 { id: String },
        Val2 { num: String },
    }

    // User-defined: SynS, SynVal

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let s = self.stack.pop().unwrap().get_s();
            self.listener.exit(Ctx::S { s });
        }
    }

// [wrapper source for test 0: PRS(34), start S]
// ------------------------------------------------------------

================================================================================
Test 1: rules RTS(21), start 0:
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(21), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], All),
// ------------------------------------------------------------
// [wrapper source for test 1: RTS(21), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, star: SynA1, c: String },
    }

    struct SynA1(Vec<String>);
    // User-defined: SynA

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 1: RTS(21), start A]
// ------------------------------------------------------------

================================================================================
Test 2: rules RTS(22), start 0:
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* | L-form (129)
            // parents:
            //  - A_1 -> A
            (RTS(22), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], All),
// ------------------------------------------------------------
// [wrapper source for test 2: RTS(22), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, star: SynA1, c: String },
    }
    pub enum CtxA1 {
        A1 { star: SynA1, b: String },
    }

    // User-defined: SynA, SynA1

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 2: RTS(22), start A]
// ------------------------------------------------------------

================================================================================
Test 3: rules RTS(32), start 0:
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
            ], All),
// ------------------------------------------------------------
// [wrapper source for test 3: RTS(32), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A1 { a: [String; 2], star: SynA1, c: String },
        A2 { a: String, c: [String; 2], star: SynA1 },
    }
    pub enum CtxA1 {
        A1 { star: SynA1, b: String },
    }

    // User-defined: SynA, SynA1

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 3: RTS(32), start A]
// ------------------------------------------------------------

================================================================================
Test 4: rules RTS(25), start 0:
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(25), 0, btreemap![
                0 => symbols![t 0, t 2],                //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a c
                1 => symbols![],                        //  1: A_1 -> # A_1 | ●A_1 ◄1 #     |
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 4: RTS(25), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, c: String },
    }

    // User-defined: SynA

    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 4: RTS(25), start A]
// ------------------------------------------------------------

================================================================================
Test 5: rules RTS(23), start 0:
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
            ], All),
// ------------------------------------------------------------
// [wrapper source for test 5: RTS(23), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, plus: SynA1, c: String },
    }

    struct SynA1(Vec<String>);
    // User-defined: SynA

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 5: RTS(23), start A]
// ------------------------------------------------------------

================================================================================
Test 6: rules RTS(27), start 0:
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
            ], All),
// ------------------------------------------------------------
// [wrapper source for test 6: RTS(27), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, plus: SynA1, c: String },
    }
    pub enum CtxB {
        B { b: String },
    }

    struct SynA1(Vec<String>);
    // User-defined: SynA, SynB

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 6: RTS(27), start A]
// ------------------------------------------------------------

================================================================================
Test 7: rules RTS(28), start 0:
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
            ], All),
// ------------------------------------------------------------
// [wrapper source for test 7: RTS(28), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { plus: SynA1, c: String },
    }
    pub enum CtxB {
        B { b: String },
    }

    struct SynA1(Vec<String>);
    // User-defined: SynA, SynB

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 7: RTS(28), start A]
// ------------------------------------------------------------

================================================================================
Test 8: rules RTS(29), start 0:
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
            ], All),
// ------------------------------------------------------------
// [wrapper source for test 8: RTS(29), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, star: SynA2, d: String },
    }
    pub enum CtxB {
        B { b: String },
    }

    struct SynA1(Vec<String>);
    struct SynA2(Vec<String>);
    // User-defined: SynA, SynB

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 8: RTS(29), start A]
// ------------------------------------------------------------

================================================================================
Test 9: rules RTS(29), start 0:
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
            ], Set(symbols![nt 0, t 0, t 2, t 3])),
// ------------------------------------------------------------
// [wrapper source for test 9: RTS(29), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, star: SynA2, d: String },
    }

    struct SynA2(Vec<String>);
    // User-defined: SynA, SynB

    enum SynValue { A(SynA), B(SynB), A2(SynA2) }

    impl SynValue {
        fn get_a(self) -> SynA {
            if let SynValue::A(val) = self { val } else { panic!() }
        }
        fn get_b(self) -> SynB {
            if let SynValue::B(val) = self { val } else { panic!() }
        }
        fn get_a2(self) -> SynA2 {
            if let SynValue::A2(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 9: RTS(29), start A]
// ------------------------------------------------------------

================================================================================
Test 10: rules RTS(29), start 0:
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
            ], Set(symbols![t 0, t 1, t 2, t 3])),
// ------------------------------------------------------------
// [wrapper source for test 10: RTS(29), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, star: SynA2, d: String },
    }
    pub enum CtxB {
        B { b: String },
    }

    struct SynA1(Vec<String>);
    struct SynA2(Vec<String>);
    // User-defined: SynA, SynB

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 10: RTS(29), start A]
// ------------------------------------------------------------

================================================================================
Test 11: rules RTS(30), start 0:
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
            ], All),
// ------------------------------------------------------------
// [wrapper source for test 11: RTS(30), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, plus: SynA2, d: String },
    }
    pub enum CtxB {
        B { b: String },
    }

    struct SynA1(Vec<String>);
    struct SynA2(Vec<String>);
    // User-defined: SynA, SynB

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 11: RTS(30), start A]
// ------------------------------------------------------------

================================================================================
Test 12: rules RTS(30), start 0:
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
            ], Set(symbols![t 0, t 1, t 2, t 3])),
// ------------------------------------------------------------
// [wrapper source for test 12: RTS(30), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, plus: SynA2, d: String },
    }
    pub enum CtxB {
        B { b: String },
    }

    struct SynA1(Vec<String>);
    struct SynA2(Vec<String>);
    // User-defined: SynA, SynB

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 12: RTS(30), start A]
// ------------------------------------------------------------

================================================================================
Test 13: rules RTS(24), start 0:
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
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 13: RTS(24), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A { a: String, plus: SynA1, c: String },
    }
    pub enum CtxA1 {
        A11 { plus: SynA1, b: String },
        A12 { plus: SynA1, b: String },
    }

    // User-defined: SynA, SynA1

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 13: RTS(24), start A]
// ------------------------------------------------------------

================================================================================
Test 14: rules PRS(28), start 0:
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
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 14: PRS(28), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A1 { e: String },
        A2 { a: String },
        A3 { a: String, b: String, c: String },
        A4 { a: String, b: String, d: String },
        A5 { a: String, b: String },
    }

    // User-defined: SynA

    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 14: PRS(28), start A]
// ------------------------------------------------------------

================================================================================
Test 15: rules PRS(31), start 0:
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec (4)
            // parents:
            //  - E_1 -> E
            (PRS(31), 0, btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ◄0 ►E_1 ►F    | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!        | id
                2 => symbols![nt 0, t 1],               //  2: E_1 -> . id E_1 | ●E_1 ◄2 id! . | E id
                3 => symbols![],                        //  3: E_1 -> ε        | ◄3            |
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 15: PRS(31), start E]

    pub enum Ctx { E { e: SynE } }
    pub enum CtxE {
        E1 { f: SynF },
        E2 { e: SynE, id: String },
        E3 { e: SynE },
    }
    pub enum CtxF {
        F { id: String },
    }

    // User-defined: SynE, SynF

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let e = self.stack.pop().unwrap().get_e();
            self.listener.exit(Ctx::E { e });
        }
    }

// [wrapper source for test 15: PRS(31), start E]
// ------------------------------------------------------------

================================================================================
Test 16: rules PRS(33), start 0:
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec (4)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (PRS(33), 0, btreemap![
                0 => symbols![],                        //  0: A -> b A_2   | ◄0 ►A_2 b! |
                1 => symbols![nt 0, t 0],               //  1: A_1 -> a A_1 | ●A_1 ◄1 a! | A a
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2         |
                3 => symbols![t 1, t 2],                //  3: A_2 -> c A_1 | ►A_1 ◄3 c! | b c
                4 => symbols![t 1, t 3],                //  4: A_2 -> d A_1 | ►A_1 ◄4 d! | b d
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 16: PRS(33), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A1 { a: SynA, a1: String },
        A2 { a: SynA },
        A3 { b: String, c: String },
        A4 { b: String, d: String },
    }

    // User-defined: SynA

    enum SynValue { A(SynA) }

    impl SynValue {
        fn get_a(self) -> SynA {
            let SynValue::A(val) = self;
            val
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 16: PRS(33), start A]
// ------------------------------------------------------------

================================================================================
Test 17: rules PRS(32), start 0:
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec | parent_left_fact (36)
            //  - E_2: child_left_fact (64)
            // parents:
            //  - E_1 -> E
            //  - E_2 -> E_1
            (PRS(32), 0, btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ◄0 ►E_1 ►F  | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!      | id
                2 => symbols![],                        //  2: E_1 -> . id E_2 | ►E_2 id! .  |
                3 => symbols![],                        //  3: E_1 -> ε        | ◄3          |
                4 => symbols![nt 0, t 1],               //  4: E_2 -> ( ) E_1  | ●E_1 ◄4 ) ( | E id
                5 => symbols![nt 0, t 1],               //  5: E_2 -> E_1      | ●E_1 ◄5     | E id
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 17: PRS(32), start E]

    pub enum Ctx { E { e: SynE } }
    pub enum CtxE {
        E1 { f: SynF },
        E2 { e: SynE },
        E3 { e: SynE },
        E4 { e: SynE, id: String },
        E5 { e: SynE, id: String },
    }
    pub enum CtxF {
        F { id: String },
    }

    // User-defined: SynE, SynF

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let e = self.stack.pop().unwrap().get_e();
            self.listener.exit(Ctx::E { e });
        }
    }

// [wrapper source for test 17: PRS(32), start E]
// ------------------------------------------------------------

================================================================================
Test 18: rules PRS(20), start 0:
            // NT flags:
            //  - LIST: right_rec (2)
            // parents:
            //  - (nothing)
            (PRS(20), 0, btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![t 5, t 5, nt 1],          //  1: LIST -> id : id ; LIST     | ◄1 ►LIST ; id! : id!  | id id LIST
                2 => symbols![],                        //  2: LIST -> }                  | ◄2 }                  |
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 18: PRS(20), start STRUCT]

    pub enum Ctx { Struct { struct: SynStruct } }
    pub enum CtxStruct {
        Struct { id: String, list: SynList },
    }
    pub enum CtxList {
        List1 { id: [String; 2], list: SynList },
        List2,
    }

    // User-defined: SynStruct, SynList

    enum SynValue { Struct(SynStruct), List(SynList) }

    impl SynValue {
        fn get_struct(self) -> SynStruct {
            if let SynValue::Struct(val) = self { val } else { panic!() }
        }
        fn get_list(self) -> SynList {
            if let SynValue::List(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let struct = self.stack.pop().unwrap().get_struct();
            self.listener.exit(Ctx::Struct { struct });
        }
    }

// [wrapper source for test 18: PRS(20), start STRUCT]
// ------------------------------------------------------------

================================================================================
Test 19: rules PRS(30), start 0:
            // NT flags:
            //  - LIST: right_rec | L-form (130)
            // parents:
            //  - (nothing)
            (PRS(30), 0, btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![nt 1, t 5, t 5],          //  1: LIST -> id : id ; LIST     | ●LIST ◄1 ; id! : id!  | LIST id id
                2 => symbols![nt 1],                    //  2: LIST -> }                  | ◄2 }                  | LIST
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 19: PRS(30), start STRUCT]

    pub enum Ctx { Struct { struct: SynStruct } }
    pub enum CtxStruct {
        Struct { id: String, list: SynList },
    }
    pub enum CtxList {
        List1 { list: SynList, id: [String; 2] },
        List2 { list: SynList },
    }

    // User-defined: SynStruct, SynList

    enum SynValue { Struct(SynStruct), List(SynList) }

    impl SynValue {
        fn get_struct(self) -> SynStruct {
            if let SynValue::Struct(val) = self { val } else { panic!() }
        }
        fn get_list(self) -> SynList {
            if let SynValue::List(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        fn exit(&mut self, _ctx: Ctx) {}
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let struct = self.stack.pop().unwrap().get_struct();
            self.listener.exit(Ctx::Struct { struct });
        }
    }

// [wrapper source for test 19: PRS(30), start STRUCT]
// ------------------------------------------------------------

================================================================================
Test 20: rules RTS(26), start 0:
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* (2560)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_left_rec (4)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (RTS(26), 0, btreemap![
                0 => symbols![t 0],                     //  0: A -> a A_2       | ◄0 ►A_2 a!      | a
                1 => symbols![nt 1, t 2],               //  1: A_1 -> c A_1     | ●A_1 ◄1 c!      | A_1 c
                2 => symbols![],                        //  2: A_1 -> ε         | ◄2              |
                3 => symbols![nt 0, nt 1, t 1],         //  3: A_2 -> A_1 b A_2 | ●A_2 ◄3 b! ►A_1 | A A_1 b
                4 => symbols![],                        //  4: A_2 -> ε         | ◄4              |
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 20: RTS(26), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A1 { a: String },
        A2 { a: SynA, star: SynA1, b: String },
        A3 { a: SynA },
    }

    struct SynA1(Vec<String>);
    // User-defined: SynA

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 20: RTS(26), start A]
// ------------------------------------------------------------

================================================================================
Test 21: rules RTS(16), start 0:
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
                0 => symbols![t 0],                     //  0: A -> a A_2       | ◄0 ►A_2 a!      | a
                1 => symbols![],                        //  1: A_1 -> c A_3     | ►A_3 c!         |
                2 => symbols![nt 0, nt 1, t 1],         //  2: A_2 -> A_1 b A_2 | ●A_2 ◄2 b! ►A_1 | A A_1 b
                3 => symbols![],                        //  3: A_2 -> ε         | ◄3              |
                4 => symbols![nt 1, t 2],               //  4: A_3 -> A_1       | ●A_1 ◄4         | A_1 c
                5 => symbols![nt 1, t 2],               //  5: A_3 -> ε         | ◄5              | A_1 c
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 21: RTS(16), start A]

    pub enum Ctx { A { a: SynA } }
    pub enum CtxA {
        A1 { a: String },
        A2 { a: SynA, plus: SynA1, b: String },
        A3 { a: SynA },
    }

    struct SynA1(Vec<String>);
    // User-defined: SynA

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let a = self.stack.pop().unwrap().get_a();
            self.listener.exit(Ctx::A { a });
        }
    }

// [wrapper source for test 21: RTS(16), start A]
// ------------------------------------------------------------

================================================================================
Test 22: rules PRS(13), start 0:
            // NT flags:
            //  - E: parent_left_rec | parent_amb (1536)
            //  - E_1: child_left_rec | child_amb (12)
            // parents:
            //  - E_1 -> E
            (PRS(13), 0, btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1     | ◄0 ►E_1 ►F   | F
                1 => symbols![nt 0],                    //  1: F -> ( E )     | ◄1 ) ►E (    | E
                2 => symbols![t 6],                     //  2: F -> N         | ◄2 N!        | N
                3 => symbols![t 7],                     //  3: F -> I         | ◄3 I!        | I
                4 => symbols![nt 0, nt 1],              //  4: E_1 -> : F E_1 | ●E_1 ◄4 ►F : | E F
                5 => symbols![nt 0, nt 1],              //  5: E_1 -> ^ F E_1 | ●E_1 ◄5 ►F ^ | E F
                6 => symbols![nt 0, nt 1],              //  6: E_1 -> / F E_1 | ●E_1 ◄6 ►F / | E F
                7 => symbols![nt 0, nt 1],              //  7: E_1 -> * F E_1 | ●E_1 ◄7 ►F * | E F
                8 => symbols![nt 0, nt 1],              //  8: E_1 -> - F E_1 | ●E_1 ◄8 ►F - | E F
                9 => symbols![nt 0, nt 1],              //  9: E_1 -> + F E_1 | ●E_1 ◄9 ►F + | E F
                10 => symbols![],                       // 10: E_1 -> ε       | ◄10          |
            ], Default),
// ------------------------------------------------------------
// [wrapper source for test 22: PRS(13), start E]

    pub enum Ctx { E { e: SynE } }
    pub enum CtxE {
        E1 { f: SynF },
        E2 { e: SynE, f: SynF },
        E3 { e: SynE, f: SynF },
        E4 { e: SynE, f: SynF },
        E5 { e: SynE, f: SynF },
        E6 { e: SynE, f: SynF },
        E7 { e: SynE, f: SynF },
        E8 { e: SynE },
    }
    pub enum CtxF {
        F1 { e: SynE },
        F2 { n: String },
        F3 { i: String },
    }

    // User-defined: SynE, SynF

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
    }

    struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LeftRecListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    impl<T: LeftRecListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        _ => panic!("unexpected exit non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
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

    impl<T: LeftRecListener> ListenerWrapper<T> {
        fn exit(&mut self, _ctx: Ctx) {
            let e = self.stack.pop().unwrap().get_e();
            self.listener.exit(Ctx::E { e });
        }
    }

// [wrapper source for test 22: PRS(13), start E]
// ------------------------------------------------------------
