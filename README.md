# Nfa_cfg_intersect

Computes the intersection between a Context-Free Grammar and a
Nondeterministic Finite Automaton

## Algorithm Overview

- Convert the CFG to a PDA
- Convert the PDA to an "sPDA" - can only push or pop a single letter,
  and can't consume and push/pop in the same transition
- Intersect the sPDA with an NFA - like intersecting 2 NFAs, but keep
  the pushes/pops from the PDA. This yields a new sPDA
- Convert this sPDA back into a CFG

## Time and Space Complexity

### Definitions

CFG

- **Var** - # of variables in the CFG
- **Der** - # of derivations in the CFG
- **Der2** - Sum of the # of variables and letters in each derivation
- **Der3** - Sum of, for each derivation, if it derives to variables,
  the # of variables + 1, and if it derives to a letter, 1

Automaton (PDA, sPDA, and NFA)

- **Sta** - # of states
- **Arr** - # of state transitions (`Arrow`s in code)
- **Arr2** - Sum of the # of pushes, pops, letters consumed, and epsilon transitions
- **Arr3** - **Sta^2 + Arr** (used for running time analysis)

### Examples

CFG: **Der** = 6, **Var** = 9

```
S -> ASA|s|_
A -> BA|a
B -> b
```

PDA: **Sta** = 3, **Arr** = 5, **Arr2** = 8, **Arr3** = 14

```
S  -[a,_->a]-> S
   +[c,_->_]-> A
   +[d,_->a]-> A
A  -[b,a->_]-> A
   +[_,_->_]-> B
B@ -*
```

Converted to sPDA: **Sta** = 6, **Arr** = 8, **Arr3** = 44

```
S  -[a]-> C
   +[c]-> A
   +[d]-> D
A  -[b]-> E
   +[_]-> B
B@ -*
C  -[+a]-> S
D  -[+a]-> A
E  -[-a]-> A
```

### Analysis

For running time, considers statements like `match _ with` and
`{ foo = bar; ... }` as **O(1)**, `Array.map` and `List.exists` as
**O(N)**, etc.

- CFG to PDA
  - 2 = **Sta** of PDA
  - **Der** of CFG = **Arr** of PDA
  - **Der3** of CFG = **Arr2** of PDA
  - **Der + Der2** of CFG ≥ **Arr2** of PDA
    - **Der + Der2 ≥ Der3**
  - **Arr2** of PDA = **O(Der2)** of CFG
  - Running time is **O(Der2)**
- PDA to sPDA
  - **Arr2** of PDA = **Arr** of sPDA
  - **Sta + (Arr2 - Arr)** of PDA = **Sta** of sPDA
  - Running time is **O(Arr2)**
- Intersect sPDA **A** and NFA **B** to form sPDA **C**
  - **Sta_A * Sta_B = Sta_C**
  - **Sta_A^2 \* Arr_B + Sta_B^2 \* Arr_A ≥ Arr_C**
  - **(Sta_A^2 + Arr_A) * Sta_B^2 ≥ Arr_C**
  - If the_ sPDA came from a CFG with **Der_A** and **Der2_A**,
    **(Der_A + Der2_A)^2 * Sta_B^2 ≥ Arr_C**
  - Running time is **O(Der2_A^2 + Sta_B^2)**
- sPDA to CFG
  - **Sta^2 + 1** of PDA = **Var** of CFG
  - **Sta^2 * (1 + Arr + Sta^2 + Sta)** of PDA ≥ **Der** of CFG
  - **2 * Sta^2 * (1 + Arr + Sta^2 + Sta)** of PDA ≥ **Der** of CFG
    - **2 * Der ≥ Der2**
  - Running time is **O(Sta^2 * (Arr + Sta^2)) = O(Sta^2 * Arr + Sta^4)**

- Intersect CFG **A** and NFA **B** to form CFG **C**
  - **2 * (Der_A + Der2_A)^4 * Sta_B^4 * (1 + 2 * (Der_A + Der2_A)^2 * Sta_B^2 + (Der_A + Der2_A)^4 * Sta_B^4) ≥ Der2_C**
  - **Der2_C = O(Der2_A^8 * Sta_B^8)**
  - Running time is **O(Der2_A^8 + Sta_B^8)**
