(** Nondeterministic Pushdown Automata.
    Implicitly starts with an empty stack, and only accepts
    when the stack is empty and at an accept state.
    Each arrow can either do nothing, consume, push, and pop one letter. *)

open Core

module Arrow = struct
  type t =
    | Epsilon
    | Consume of Letter.t
    | Push of Letter.t
    | Pop of Letter.t

  let print arr =
    let internal =
      match arr with
      | Epsilon -> Letter.to_string Letter.epsilon
      | Consume l -> Letter.to_string l
      | Push l -> "+" ^ Letter.to_string l
      | Pop l -> "-" ^ Letter.to_string l in
    "[" ^ internal ^ "]"
end

include Automaton.Make(Arrow)