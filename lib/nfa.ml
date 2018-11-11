(** Nondeterministic Finite Automata.
    Each arrow can only consume one letter or epsilon. *)

open Core

module Arrow = struct
  type t =
    { consume : Letter.t;
    }

  let print arr =
    Letter.print arr.consume
end

include Automaton.Make(Arrow)
