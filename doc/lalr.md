```
0: s -> "a" a "a"
1: s -> "a" "a" "b"
2: s -> "b" a "b"
3: a -> b c
4: b -> "a"
5: c -> d
6: d -> 
7: <goal> -> s
```

States:
```
state 0 ------------+-"a"-> state 1 -----------+-"a"-> state 4 ------------"b"-> state 9 
<goal> -> • s       |       s -> "a" • a "a"   |       s -> "a" "a" • "b"        s -> "a" "a" "b" •
s -> • "a" a "a"    |       s -> "a" • "a" "b" |       b -> "a" •
s -> • "a" "a" "b"  |       a -> • b c         |
s -> • "b" a "b"    |       b -> • "a"         +--a--> state 5 ------------"a"-> state 10
                    |                          |       s -> "a" a • "a"          s -> "a" a "a" •
                    |                          |
                    |                          +--b--> state 6 ----------+--c--> state 11
                    |                                  a -> b • c        |       a -> b c •
                    |                          +--b--> c -> • d          |
                    |                          |       d -> •            +--d--> state 12
                    |                          |                                 c -> d •
                    |                          |
                    +-"b"-> state 2 -----------+-"a"-> state 7
                    |       s -> "b" • a "b"   |       b -> "a" •
                    |       a -> • b c         |       
                    |       b -> • "a"         +--a--> state 8 ------------"b"-> state 13
                    |                                  s -> "b" a • "b"          s -> "b" a "b" •
                    +--s--> state 3
                            <goal> -> s • ----> accept

gotos:                                        |   rev_gotos:
----------------------------------------------+-----------------------------------
- 0: "a" → 1, "b" → 2, s → 3   - 7:           |   - 0:              - 7: "a" → {2}
- 1: "a" → 4, a → 5, b → 6     - 8: "b" → 13  |   - 1: "a" → {0}    - 8: a → {2}
- 2: "a" → 7, a → 8, b → 6     - 9:           |   - 2: "b" → {0}    - 9: "b" → {4}
- 3:                           - 10:          |   - 3: s → {0}      - 10: "a" → {5}
- 4: "b" → 9                   - 11:          |   - 4: "a" → {1}    - 11: c → {6}
- 5: "a" → 10                  - 12:          |   - 5: a → {1}      - 12: d → {6}
- 6: c → 11, d → 12            - 13:          |   - 6: b → {1, 2}   - 13: "b" → {8}
```

G', where NT and T are transitions in G:
```
NT: [0:s] [1:a] [2:a] [2:b] [6:c] [6:d]
T:  [0:"a"] [0:"b"] [1:"a"] [2:"a"] [4:"b"] [5:"a"] [8:"b"]

0: [0:s] -> [0:"a"] [1:a] [5:"a"]
1: [0:s] -> [0:"a"] [1:"a"] [4:"b"]
2: [0:s] -> [0:"b"] [2:a] [8:"b"]
3: [1:a] -> [1:b] [6:c]
4: [1:b] -> [1:"a"]
5: [2:a] -> [2:b] [6:c]
6: [2:b] -> [2:"a"]
7: [6:c] -> [6:d]
8: [6:d] -> 
```

Follow:
```
-[0:s] -> $
-[1:a] -> [5:"a"]
-[1:b] -> [5:"a"]
-[2:a] -> [8:"b"]
-[2:b] -> [8:"b"]
-[6:c] -> [5:"a"], [8:"b"]
-[6:d] -> [5:"a"], [8:"b"]
```

States with lookahead.

For a reduction (state q, item `s -> ω •`), where ω is a string of grammar symbols, the lookahead set is defined by
```
{ t ∈ T: ∃ state p:
- state' r: [r:t] ∈ follow [p:s]
- Go [p:ω] = q
}
```
where `Go [p:ω]` is the state obtained by starting at state p and following the transitions for each symbol of ω.

There are three ways to compute them. For example, for `(q = 10, s -> "a" a "a" •)`
1) Forward:
   - For each state s' in G' that correspond to the nonterminal `s`; here, `[0:s]`:
     - Extract the corresponding states in G. Here, 0.
     - For each state, follow the transitions of the symbols in ω. Here, 0 -> 1 -> 5 -> 10. 
     - if the final state is q, extract the lookahead from the follow set of s'. Here, `$` (in others like `[5:"a"], [8:"b"]`, extract the terminals: `"a","b"`) 
2) Backward:
   - Starting from state q, follow all the possible transitions of the symbols in ω, backwards from the end. Here, 10 -> 5 -> 1 -> 0 (there could be several paths). 
     - extract the lookahead from the follow set of `[p:s]` where p is the final state (0).
3) Shortcut (requires an extra table with equivalent productions in G' to avoid more processing):
   - For each production P' in G' `s' -> ω'` that corresponds to `s -> ω`:
     - take the last symbol of P', extract the state s(n) and make the last transition on `ω(n)`. Here, `[0:s] -> [0:"a"] [1:a] >>> [5:"a"] <<<`, and 5 --"a"--> 10.
     - if the final state is q, extract the lookahead from the follow set of s'. Here, `$` (in others like `[5:"a"], [8:"b"]`, extract the terminals: `"a","b"`)

Note: The lookahead of the accepting state is `$`.

```
state 0:
  - <goal> -> • s
  - s -> • "a" a "a"
  - s -> • "a" "a" "b"
  - s -> • "b" a "b"
state 1:
  - s -> "a" • a "a"
  - s -> "a" • "a" "b"
  - a -> • b c
  - b -> • "a"
state 2:
  - s -> "b" • a "b"
  - a -> • b c
  - b -> • "a"
state 3:
  - <goal> -> s •
state 4:
  - s -> "a" "a" • "b"
  - b -> "a" •, ["a"]
state 5:
  - s -> "a" a • "a"
state 6:
  - a -> b • c
  - c -> • d
  - d -> •, ["a","b"]
state 7:
  - b -> "a" •, ["b"]
state 8:
  - s -> "b" a • "b"
state 9:
  - s -> "a" "a" "b" •, [$]
state 10:
  - s -> "a" a "a" •, [$]
state 11:
  - a -> b c •, ["a","b"]
state 12:
  - c -> d •, ["a","b"]
state 13:
  - s -> "b" a "b" •, [$]
```