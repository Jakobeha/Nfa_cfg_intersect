# Nfa_cfg_intersect

Computes the intersection between a Context-Free Grammar and a
Nondeterministic Finite Automaton

## How To Use

- Run the command-line program at `out/nfa_cfg_intersect`
- Type the CFG into the command-line, followed by `\n===\n`, followed by
  the NFA. Then type `Ctrl-D` to signal end-of-input
- The program should print out the intersection of the NFA and CFG

Run `out/nfa_cfg_intersect --help` for more info.

## Syntax

### Grammar

```ebnf
<CFG> ::= <VAR> { '\n' <VAR> }
<VAR> ::= <ID> ' -> ' <DER> { '|' <DER> }
<DER> ::= <ID> { <ID> }
        | <LETTER>

<NFA> ::= <AUTO(NFA_ARR)>
<PDA> ::= <AUTO(PDA_ARR)>
<AUTO(ARR)> ::= <STA(ARR)> { '\n' <STA(ARR)> }
<STA(ARR)> ::= <ID> <ACCEPT?> ' -' <ARR> '-> ' <ID> { '\n   +' <ARR> '-> ' <ID> }
<NFA_ARR> ::= <LETTER>
<PDA_ARR> ::= '[' <LETTER> ',' <STK_LETTERS> '->' <STK_LETTERS> ']'
<STK_LETTERS> ::= <STK_LETTER> { <STK_LETTER> }
                | '_'
<STK_LETTER> ::= 'A' - 'Z'
               | 'a' - 'z'

<ID>  ::= 'A' - 'Z'
<LETTER> ::= ( 'a' - 'z' | '_' )
```

#### Informal

- In CFGs, lowercase letters are terminals, and uppercase letters are variables
  (nonterminals). In automata, lowercase letters are symbols, and uppercase
  letters are state IDs. The "alphabet" of both is the lowercase English
  alphabet. However, the alphabet of the PDA's stack also contains uppercase
  letters, which are used in CFG-PDA conversion to represent the CFG's nonterminals
  (not to be confused with a nonterminals or state IDs themselves).
- Underscore `_` is a stand-in for epsilon `ε`. Underscore is used instead
  because ocammlex doesn't support unicode, and it's easier to type.
- `S` is the start state in an automata, and the start variable in a CFG.
- The syntax of NFAs is essentially a set of transitions: `A -a-> B` represents a transition from state A to B consuming the letter "a", and `B -_-> C` represents a transition from state B to C not consuming anything (epsilon), etc. The pluses define multiple transitions from the same state, e.g.

  ```
  A  -a-> B
     +b-> C
     +c-> D
  ```

  is roughly equivalent to

  ```
  A  -a-> B
  A  -b-> C
  A  -c-> D
  ```

  (except the syntax doesn't allow the latter).
- Define a state with no transitions like so: `A -*`
- A state is an accept state if it has an `@` after it, e.g. in

  ```
  S  -a-> B
  A@ -b-> S
     +c-> B
  B  -b-> A
     +c-> C
  C@ -*
  ```

  A and C are accept states (and C has no transitions).
- The syntax of PDAs is very similar to NFAs. The only difference is that,
  for each transition, you don't just specify a letter to consume, you also
  specify what letters to push and pop off the stack. For example:

  ```
  S  -[a,_->b]-> A
     +[_,c->def]-> G
  ```

  Specifies that the state `S` has 2 transitions. The first transition
  consumes the letter `a`, pushes `b` onto the stack, and moves to state
  `A`. The second transition doesn't consume anything (epsilon), pops `c`
  from the stack, and pushes `d`, `e`, and `f`, *in that order* (so `f` will
  be at the top of the stack).

### Example

The following

```
> nfa_cfg_intersect
S -> _
===
S@ -*
```

should output

```
S -> B
A -> _
B -> D|AB|BD
C -> CA|DC
D -> _
```

### Tips

If you're getting syntax errors:

- Whitespace is important. Don't have any extra spaces or newlines.
- Make sure the first variable or state is `S`, and the rest are the
  uppercase letters in order.

  ```
  B -> b
  A -> a
  S -> A|B
  ```

  is invalid, it should be

  ```
  S -> A|B
  A -> a
  B -> b
  ```

- Each derivation in a CFG must be either a single letter or a sequence
  of variables. `S -> aBC` is invalid, do `S -> ABC` and then `A -> a`.

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

Automaton (PDA, sPDA, and NFA)

- **Sta** - # of states
- **Arr** - # of state transitions (`Arrow`s in code)
- **Arr2** - Sum of the # of pushes, pops, letters consumed, and epsilon transitions

#### Examples

CFG: **Der** = 6, **Var** = 9

```
S -> ASA|s|_
A -> BA|a
B -> b
```

NFA: **Sta** = 2, **Arr** = 3

```
S  -a-> A
A@ -_-> S
   +b-> A
```

PDA: **Sta** = 3, **Arr** = 5, **Arr2** = 8

```
S  -[a,_->a]-> S
   +[c,_->_]-> A
   +[d,_->a]-> A
A  -[b,a->_]-> A
   +[_,_->_]-> B
B@ -*
```

Converted to sPDA: **Sta** = 6, **Arr** = 8

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

For running time, statements like `match _ with` and `{ foo = bar; ... }`
are considered **O(1)**, and statements like `Array.map` and `List.exists`
are considered **O(NF)**, where **N** is the size of the input and **F**
is the running time of the provided function.

Full algorithm:

- Intersect CFG **A** and NFA **B** to form CFG **C**
  - **Der2_C = O(Der2_A^8 * Sta_B^8)**
  - Running time is **O(Der2_A^8 + Sta_B^8)**

Steps:

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
