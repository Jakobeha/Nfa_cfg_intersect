(** Nondeterministic Pushdown Automata.
    Implicitly starts with an empty stack, and only accepts
    when the stack is empty and at an accept state.
    Each arrow can only consume one letter or epsilon,
    but can pop or push multiple letters -
    can pop and push epsilons (which doesn't do anything). *)

open Core

module Arrow = struct
  type t =
    { consume : Letter.t;
      pops : Word.t;
      pushes : Word.t;
    }

  let print arr =
    "[" ^
    Letter.print arr.consume ^
    "," ^
    Word.print_nonempty arr.pops ^
    "->" ^
    Word.print_nonempty arr.pushes ^
    "]"
end

include Automaton.Make(Arrow)
