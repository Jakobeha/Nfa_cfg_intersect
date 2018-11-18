(** Nondeterministic Finite Automata.
    Each arrow can only consume one letter or epsilon. *)

open Core

module Memory = struct
  type t = unit

  let init = ()

  let can_accept () = true
end

module Arrow = struct
  type t =
    { consume : Letter.t;
    }

  let follow () l arr =
    if arr.consume = Letter.epsilon then
      Automaton.Checkpoint.Continue ((), 0)
    else if arr.consume = l then
      Continue ((), 1)
    else
      Reject

  let print arr =
    Letter.print arr.consume
end

include Automaton.Make(Memory)(Arrow)
