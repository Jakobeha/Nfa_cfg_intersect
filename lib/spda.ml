(** Nondeterministic Pushdown Automata with up to 1 pop/push per arrow.
    Implicitly starts with an empty stack, and only accepts
    when the stack is empty and at an accept state.
    Each arrow can only consume, push, and pop one letter or epsilon. *)

open Core

module Arrow = struct
  type t =
    { consume : Letter.t;
      pop : Letter.t;
      push : Letter.t;
    }

  let print arr =
    "[" ^
    Letter.print arr.consume ^
    "," ^
    Letter.print arr.pop ^
    "->" ^
    Letter.print arr.push ^
    "]"
end

include Automaton.Make(Arrow)
