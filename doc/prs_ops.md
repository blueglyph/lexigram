# Base rules

--------------------------------------------------------------------------------

## Trivial rule

<!-- 13 -->

```
s -> Id "=" val | "exit" | "return" val
val -> Id | Num
```

```
0: s -> Id "=" val   | ◄0 ►val "=" Id!  | Id val
1: s -> "exit"       | ◄1 "exit"        |
2: s -> "return" val | ◄2 ►val "return" | val
3: val -> Id         | ◄3 Id!           | Id
4: val -> Num        | ◄4 Num!          | Num
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
    fn exit_s(&mut self, ctx: CtxS) -> SynS;
    fn init_val(&mut self) {}
    fn exit_val(&mut self, ctx: CtxVal) -> SynVal;
}
```

## Repetitions with *

<!-- 102 -->

```
a -> A B* C
```

```
0: a -> A a_1 C | ◄0 C! ►a_1 A! | A a_1 C
1: a_1 -> B a_1 | ●a_1 ◄1 B!    | a_1 B
2: a_1 -> ε     | ◄2            | a_1
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
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

## Repetitions with +

<!-- 103 -->

```
a -> A B+ C
```

```
0: a -> A a_1 C | ◄0 C! ►a_1 A! | A a_1 C
1: a_1 -> B a_2 | ►a_2 B!       |
2: a_2 -> a_1   | ●a_1 ◄2       | a_1 B
3: a_2 -> ε     | ◄3            | a_1 B
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
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

## Repetitions with * and `<L>` attribute

<!-- 200 -->

```
a -> A (<L=i> B)* C
```

```
0: a -> A i C | ◄0 C! ►i A! | A C
1: i -> B i   | ●i ◄1 B!    | B
2: i -> ε     | ◄2          |
```

```rust
pub enum CtxA {
    /// `a -> A (<L> B)* C`
    V1 { a: String, star: SynI, c: String },
}

pub enum CtxI {
    /// `<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )* C`
    V1 { star_acc: SynI, b: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
    fn init_i(&mut self) -> SynI;
    fn exit_i(&mut self, ctx: CtxI) -> SynI;
    fn exitloop_i(&mut self, _star_acc: &mut SynI) {}
}
```

## Repetitions with + and `<L>` attribute

<!-- 201 -->

```
a -> A (<L=i> B)+ C
```

```
0: a -> A i C | ◄0 C! ►i A! | A i C
1: i -> B a_1 | ►a_1 B!     |
2: a_1 -> i   | ●i ◄2       | i B
3: a_1 -> ε   | ◄3          | i B  
```

```rust
pub enum CtxA {
    /// `a -> A (<L> B)+ C`
    V1 { a: String, plus: SynI, c: String },
}

pub enum CtxI {
    /// `<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`
    V1 { plus_acc: SynI, b: String, last_iteration: bool },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynMyA;
    fn init_i(&mut self) -> SynI;
    fn exit_i(&mut self, ctx: CtxI) -> SynI;
}
```

## Right recursion

<!-- 301 -->

```
expr -> Id "." expr | "(" Num ")"
```

```
0: expr -> Id "." expr | ◄0 ►expr "." Id! | Id
1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | Num
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
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
}
```

## Right recursion with `<L>` attribute

<!-- 401 -->

```
expr -> <L> Id "." expr | "(" Num ")"
```

```
0: expr -> Id "." expr | ●expr ◄0 "." Id! | expr Id
1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | expr Num
```

```rust
pub enum CtxExpr {
    /// `expr -> <L> Id "." expr`
    V1 { expr: SynExpr, id: String },
    /// `expr -> "(" Num ")"`
    V2 { expr: SynExpr, num: String },
}

pub trait TestListener {
    // ...
    fn init_expr(&mut self) -> SynExpr;
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
}
```

## Left recursion

<!-- 502 -->

```
e -> f | e "." Id
```

```
0: e -> f e_1        | ►e_1 ◄0 ►f      | f
1: f -> Id           | ◄1 Id!          | Id
2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | e Id
3: e_1 -> ε          | ◄3              | e         
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
    fn exit_e(&mut self, ctx: CtxE) -> SynE;
    fn exitloop_e(&mut self, _e: &mut SynE) {}
}
```

## Left factorization

<!-- 705 -->

```
a -> A | A B | A B C | A B D | E
```

```
0: a -> A a_1   | ►a_1 A! |
1: a -> E       | ◄1 E!   | E
2: a_1 -> B a_2 | ►a_2 B! |
3: a_1 -> ε     | ◄3      | A
4: a_2 -> C     | ◄4 C!   | A B C
5: a_2 -> D     | ◄5 D!   | A B D
6: a_2 -> ε     | ◄6      | A B    
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
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
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
0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
2: e_1 -> "!" e_2 e_1 | ●e_1 ◄2 ►e_2 "!" | e e
3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | e e
4: e_1 -> ε           | ◄4               | e
5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | e
6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | e e
7: e_3 -> "!" e_2 e_3 | ●e_3 ◄7 ►e_2 "!" | e e
8: e_3 -> ε           | ◄8               | e
9: e_4 -> Num         | ◄9 Num!          | Num        
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
    fn exit_e(&mut self, ctx: CtxE) -> SynE;
}
```

## Repetitions of longer strings of symbols

<!-- 105 -->

```
a -> (b A b B A)+
```

```
0: a -> a_1             | ◄0 ►a_1             | a_1
1: b -> C               | ◄1 C!               | C
2: a_1 -> b A b B A a_2 | ►a_2 A! B! ►b A! ►b |
3: a_2 -> a_1           | ●a_1 ◄3             | a_1 b A b B A
4: a_2 -> ε             | ◄4                  | a_1 b A b B A            
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
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

## Nested repetitions

<!-- 106 -->

```
a -> (A (b ",")* ";")* C
```

```
0: a -> a_2 C           | ◄0 C! ►a_2          | a_2 C
1: b -> B               | ◄1 B!               | B
2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | a_1 b
3: a_1 -> ε             | ◄3                  | a_1
4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | a_2 A a_1
5: a_2 -> ε             | ◄5                  | a_2            
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
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

## Alternatives in repetitions

<!-- 152 -->

```
a -> A (B | b C b B C | E)* F
```

```
0: a -> A a_1 F         | ◄0 F! ►a_1 A!          | A a_1 F
1: b -> D               | ◄1 D!                  | D
2: a_1 -> B a_1         | ●a_1 ◄2 B!             | a_1 B
3: a_1 -> b C b B C a_1 | ●a_1 ◄3 C! B! ►b C! ►b | a_1 b C b B C
4: a_1 -> E a_1         | ●a_1 ◄4 E!             | a_1 E
5: a_1 -> ε             | ◄5                     | a_1            
```

```rust
pub enum CtxA {
    /// `a -> A (B | b C b B C | E)* F`
    V1 { a: String, star: SynA1, f: String },
}

pub enum CtxB {
    /// `b -> D`
    V1 { d: String },
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
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```
