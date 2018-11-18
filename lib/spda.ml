(** Nondeterministic Pushdown Automata.
    Implicitly starts with an empty stack, and only accepts
    when the stack is empty and at an accept state.
    Each arrow can either do nothing, consume, push, and pop one letter. *)

open Core

module Memory = struct
  type t = Letter.t List.t

  let init = []

  let can_accept stk = List.is_empty stk
end

module Arrow = struct
  type t =
    | Epsilon
    | Consume of Letter.t
    | Push of Letter.t
    | Pop of Letter.t

  let follow stk l arr =
    match arr with
    | Epsilon ->
      Automaton.Checkpoint.Continue (stk, 0)
    | Consume csm ->
      if csm <> l then
        Reject
      else
        Continue (stk, 1)
    | Push psh ->
      Continue (psh :: stk, 0)
    | Pop pop ->
      match stk with
      | [] -> Reject
      | fst :: rst ->
        if fst <> pop then
          Reject
        else
          Continue (rst, 0)

  let print arr =
    let internal =
      match arr with
      | Epsilon -> Letter.print Letter.epsilon
      | Consume l -> Letter.print l
      | Push l -> "+" ^ Letter.print l
      | Pop l -> "-" ^ Letter.print l in
    "[" ^ internal ^ "]"
end

include Automaton.Make(Memory)(Arrow)
