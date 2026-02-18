# Base rules

--------------------------------------------------------------------------------

## Trivial rule

<!-- 13 -->

```
s -> Id "=" val | "exit" | "return" val
val -> Id | Num
```

```
0: s -> Id "=" val   | ◄0 ►val "=" Id!  | 3 | Id val
1: s -> "exit"       | ◄1 "exit"        | 1 |
2: s -> "return" val | ◄2 ►val "return" | 2 | val
3: val -> Id         | ◄3 Id!           | 1 | Id
4: val -> Num        | ◄4 Num!          | 1 | Num
```

```rust
pub enum CtxS {
    /// `s -> Id "=" val`
    V1 { id: String, val: SynVal },
    /// `s -> "exit"`
    V2,
    /// `s -> "return" val`
    V3 { val: SynVal },
}

pub enum CtxVal {
    /// `val -> Id`
    V1 { id: String },
    /// `val -> Num`
    V2 { num: String },
}

pub trait TestListener {
    // ...
    fn init_s(&mut self) {}
    fn exit_s(&mut self, ctx: CtxS, spans: Vec<PosSpan>) -> SynS;
    fn init_val(&mut self) {}
    fn exit_val(&mut self, ctx: CtxVal, spans: Vec<PosSpan>) -> SynVal;
}
```

## Repetitions with *

<!-- 102 -->

```
a -> A B* C
```

```
0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
1: a_1 -> B a_1 | ●a_1 ◄1 B!    | 2 | a_1 B
2: a_1 -> ε     | ◄2            | 1 | a_1
```

```rust
pub enum CtxA {
    /// `a -> A B* C`
    V1 { a: String, star: SynA1, c: String },
}

pub struct SynA1(pub Vec<String>);

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
}
```

### * with sep_list

<!-- 109 -->

```
a -> Id "(" Id ":" type ("," Id ":" type)* ")"
type -> Id
```

```
0: a -> Id "(" Id ":" type a_1 ")" | ◄0 ")" ►a_1 ►type ":" Id! "(" Id! | 4    | Id a_1
1: type -> Id                      | ◄1 Id!                            | 1    | Id
2: a_1 -> "," Id ":" type a_1      | ●a_1 ◄2 ►type ":" Id! ","         | 5, 3 | a_1 Id type
3: a_1 -> ε                        | ◄3                                | 1    | a_1
```

Note there aren't `id` and `type` fields in `CtxA`. They're now in `star[0]`. 
```rust
pub enum CtxA {
    /// `a -> Id "(" Id ":" type ("," Id ":" type)* ")"`
    V1 { id: String, star: SynA1 },
}

pub enum CtxType {
    /// `type -> Id`
    V1 { id: String },
}

pub struct SynA1(pub Vec<SynA1Item>);
pub struct SynA1Item { pub id: String, pub type1: SynType }

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
    fn init_type(&mut self) {}
    fn exit_type(&mut self, ctx: CtxType, spans: Vec<PosSpan>) -> SynType;
}
```

## Repetitions with +

<!-- 103 -->

```
a -> A B+ C
```

```
0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
1: a_1 -> B a_2 | ►a_2 B!       | 0 |
2: a_2 -> a_1   | ●a_1 ◄2       | 2 | a_1 B
3: a_2 -> ε     | ◄3            | 2 | a_1 B
```

```rust
pub enum CtxA {
    /// `a -> A B+ C`
    V1 { a: String, plus: SynA1, c: String },
}

/// Computed `B+` array in `a -> A  ►► B+ ◄◄  C`
pub struct SynA1(pub Vec<String>);

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
}
```

Note: `sep_list` transformations are not performed on `+`

## Repetitions with * and `<L>` attribute

<!-- 200 -->

```
a -> A (<L=i> B)* C
```

```
0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
1: i -> B i   | ●i ◄1 B!    | 2 | i B
2: i -> ε     | ◄2          | 1 | i
```

```rust
pub enum CtxA {
    /// `a -> A (<L> B)* C`
    V1 { a: String, star: SynI, c: String },
}

pub enum CtxI {
    /// `<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )* C`
    V1 { b: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
    fn init_i(&mut self) -> SynI;
    fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI, spans: Vec<PosSpan>);
    fn exitloop_i(&mut self, acc: &mut SynI) {}
}
```

### `<L>`* with sep_list

<!-- 212 -->

```
a -> Id "(" Id ":" type (<L=i> "<" ">" Id ":" type)* ")"
```

```
0: a -> Id "(" Id ":" type i ")" | ◄0 ")" ►i ►type ":" Id! "(" Id! | 4    | Id i
1: i -> "<" ">" Id ":" type i    | ●i ◄1 ►type ":" Id! ">" "<"     | 6, 3 | i Id type
2: i -> ε                        | ◄2                              | 1    | i
3: type -> Id                    | ◄3 Id!                          | 1    | Id
```

```rust
pub enum CtxA {
    /// `a -> Id "(" Id ":" type (<L> "<" ">" Id ":" type)* ")"`
    V1 { id: String, star: SynI },
}

pub enum InitCtxI {
    /// value of `Id type` before `<L> "<" ">" Id ":" type` iteration in `a -> Id "(" Id ":" type ( ►► <L> "<" ">" Id ":" type ◄◄ )* ")"`
    V1 { id: String, type1: SynType },
}

pub enum CtxI {
    /// `<L> "<" ">" Id ":" type` iteration in `a -> Id "(" Id ":" type ( ►► <L> "<" ">" Id ":" type ◄◄ )* ")"`
    V1 { id: String, type1: SynType },
}

pub enum CtxType {
    /// `type -> Id`
    V1 { id: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
    fn init_i(&mut self, ctx: InitCtxI, spans: Vec<PosSpan>) -> SynI;
    fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI, spans: Vec<PosSpan>);
    fn exitloop_i(&mut self, acc: &mut SynI) {}
    fn init_type(&mut self) {}
    fn exit_type(&mut self, ctx: CtxType, spans: Vec<PosSpan>) -> SynType;
}
```

## Repetitions with + and `<L>` attribute

<!-- 201 -->

```
a -> A (<L=i> B)+ C
```

```
0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
1: i -> B i_1 | ►i_1 B!     | 0 |
2: i_1 -> i   | ●i ◄2       | 2 | i B
3: i_1 -> ε   | ◄3          | 2 | i B
```

```rust
pub enum CtxA {
    /// `a -> A (<L> B)+ C`
    V1 { a: String, plus: SynI, c: String },
}

pub enum CtxI {
    /// `<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`
    V1 { b: String, last_iteration: bool },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
    fn init_i(&mut self) -> SynI;
    fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI, spans: Vec<PosSpan>);
}
```

## Right recursion

<!-- 301 -->

```
expr -> Id "." expr | "(" Num ")"
```

```
0: expr -> Id "." expr | ◄0 ►expr "." Id! | 3 | Id expr
1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 3 | Num
```

```rust
pub enum CtxExpr {
    /// `expr -> Id "." expr`
    V1 { id: String, expr: SynExpr },
    /// `expr -> "(" Num ")"`
    V2 { num: String },
}

pub trait TestListener {
    // ...
    fn init_expr(&mut self) {}
    fn exit_expr(&mut self, ctx: CtxExpr, spans: Vec<PosSpan>) -> SynExpr;
}
```

## Right recursion with `<L>` attribute

<!-- 401 -->

```
expr -> <L> Id "." expr | "(" Num ")"
```

```
0: expr -> Id "." expr | ●expr ◄0 "." Id! | 3 | expr Id
1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 4 | expr Num
```

```rust
pub enum CtxExpr {
    /// `expr -> <L> Id "." expr`
    V1 { id: String },
    /// `expr -> "(" Num ")"`
    V2 { num: String },
}

pub trait TestListener {
    // ...
    fn init_expr(&mut self) -> SynExpr;
    fn exit_expr(&mut self, acc: &mut SynExpr, ctx: CtxExpr, spans: Vec<PosSpan>);
}
```

## Left recursion

<!-- 502 -->

```
e -> f | e "." Id
```

```
0: e -> f e_1        | ►e_1 ◄0 ►f      | 1 | f
2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | 3 | e Id
3: e_1 -> ε          | ◄3              | 1 | e         
```

```rust
pub enum CtxE {
    /// `e -> f`
    V1 { f: SynF },
    /// `e -> e "." Id`
    V2 { e: SynE, id: String },
}

pub trait TestListener {
    // ...
    fn init_e(&mut self) {}
    fn exit_e(&mut self, ctx: CtxE, spans: Vec<PosSpan>) -> SynE;
    fn exitloop_e(&mut self, e: &mut SynE) {}
}
```

## Left factorization

<!-- 705 -->

```
a -> A | A B | A B C | A B D | E
```

```
0: a -> A a_1   | ►a_1 A! | 0 |
1: a -> E       | ◄1 E!   | 1 | E
2: a_1 -> B a_2 | ►a_2 B! | 0 |
3: a_1 -> ε     | ◄3      | 1 | A
4: a_2 -> C     | ◄4 C!   | 3 | A B C
5: a_2 -> D     | ◄5 D!   | 3 | A B D
6: a_2 -> ε     | ◄6      | 2 | A B    
```

```rust
pub enum CtxA {
    /// `a -> A`
    V1 { a: String },
    /// `a -> A B`
    V2 { a: String, b: String },
    /// `a -> A B C`
    V3 { a: String, b: String, c: String },
    /// `a -> A B D`
    V4 { a: String, b: String, d: String },
    /// `a -> E`
    V5 { e: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
}
```

# Advanced rules

--------------------------------------------------------------------------------

## Ambiguous left and right recursion

<!-- 607 -->

```
e -> e "*" e | <R> e "!" e | e "+" e | Num
```

```
0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
2: e_1 -> "!" e_2 e_1 | ●e_1 ◄2 ►e_2 "!" | 3 | e e
3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
4: e_1 -> ε           | ◄4               | 1 | e
5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
7: e_3 -> "!" e_2 e_3 | ●e_3 ◄7 ►e_2 "!" | 3 | e e
8: e_3 -> ε           | ◄8               | 1 | e
9: e_4 -> Num         | ◄9 Num!          | 1 | Num        
```

```rust
pub enum CtxE {
    /// `e -> e "*" e`
    V1 { e: [SynE; 2] },
    /// `e -> <R> e "!" e`
    V2 { e: [SynE; 2] },
    /// `e -> e "+" e`
    V3 { e: [SynE; 2] },
    /// `e -> Num`
    V4 { num: String },
}

pub trait TestListener {
    // ...
    fn init_e(&mut self) {}
    fn exit_e(&mut self, ctx: CtxE, spans: Vec<PosSpan>) -> SynE;
}
```

## Repetitions of longer strings of symbols

<!-- 105 -->

```
a -> (b A b B A)+
```

```
0: a -> a_1             | ◄0 ►a_1             | 1 | a_1
2: a_1 -> b A b B A a_2 | ►a_2 A! B! ►b A! ►b | 0 |
3: a_2 -> a_1           | ●a_1 ◄3             | 6 | a_1 b A b B A
4: a_2 -> ε             | ◄4                  | 6 | a_1 b A b B A            
```

```rust
pub enum CtxA {
    /// `a -> (b A b B A)+`
    V1 { plus: SynA1 },
}

/// Computed `(b A b B A)+` array in `a ->  ►► (b A b B A)+ ◄◄ `
pub struct SynA1(pub Vec<SynA1Item>);

/// `b A b B A` item in `a -> ( ►► b A b B A ◄◄ )+`
pub struct SynA1Item { pub b: [SynB; 2], pub a: [String; 2], pub b1: String }

pub trait TestListener {
    // ..
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
}
```

## Nested repetitions

<!-- 106 -->

```
a -> (A (b ",")* ";")* C
```

```
0: a -> a_2 C           | ◄0 C! ►a_2          | 2 | a_2 C
2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | 3 | a_1 b
3: a_1 -> ε             | ◄3                  | 1 | a_1
4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | 4 | a_2 A a_1
5: a_2 -> ε             | ◄5                  | 1 | a_2            
```

```rust
pub enum CtxA {
    /// `a -> (A (b ",")* ";")* C`
    V1 { star: SynA2, c: String },
}

/// Computed `(b ",")*` array in `a -> (A  ►► (b ",")* ◄◄  ";")* C`
pub struct SynA1(pub Vec<SynB>);

/// Computed `(A (b ",")* ";")*` array in `a ->  ►► (A (b ",")* ";")* ◄◄  C`
pub struct SynA2(pub Vec<SynA2Item>);

/// `A (b ",")* ";"` item in `a -> ( ►► A (b ",")* ";" ◄◄ )* C`
pub struct SynA2Item { pub a: String, pub star: SynA1 }

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
}
```

## Alternatives in repetitions

<!-- 152 -->

```
a -> A (B | b C b B C | E)* F
```

```
0: a -> A a_1 F         | ◄0 F! ►a_1 A!          | 3 | A a_1 F
2: a_1 -> B a_1         | ●a_1 ◄2 B!             | 2 | a_1 B
3: a_1 -> b C b B C a_1 | ●a_1 ◄3 C! B! ►b C! ►b | 6 | a_1 b C b B C
4: a_1 -> E a_1         | ●a_1 ◄4 E!             | 2 | a_1 E
5: a_1 -> ε             | ◄5                     | 1 | a_1            
```

```rust
pub enum CtxA {
    /// `a -> A (B | b C b B C | E)* F`
    V1 { a: String, star: SynA1, f: String },
}

/// Computed `(B | b C b B C | E)*` array in `a -> A  ►► (B | b C b B C | E)* ◄◄  F`
pub struct SynA1(pub Vec<SynA1Item>);

pub enum SynA1Item {
    /// `B` item in `a -> A ( ►► B ◄◄  | b C b B C | E)* F`
    V1 { b: String },
    /// `b C b B C` item in `a -> A (B |  ►► b C b B C ◄◄  | E)* F`
    V2 { b: [SynB; 2], c: [String; 2], b1: String },
    /// `E` item in `a -> A (B | b C b B C |  ►► E ◄◄ )* F`
    V3 { e: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
}
```
