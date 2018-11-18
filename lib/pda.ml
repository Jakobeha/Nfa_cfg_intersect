(** Nondeterministic Pushdown Automata with multiple pops/pushes per arrow.
    Implicitly starts with an empty stack, and only accepts
    when the stack is empty and at an accept state.
    Each arrow can only consume one letter or epsilon,
    but can pop or push multiple letters -
    can pop and push epsilons (which doesn't do anything). *)

open Core

module Memory = struct
  type t = Letter.t List.t

  let init = []

  let can_accept stk = List.is_empty stk
end

module Arrow = struct
  type t =
    { consume : Letter.t;
      pops : Word.t;
      pushes : Word.t;
    }

  let follow stk l arr =
    let eps = arr.consume = Letter.epsilon in
    if not eps && arr.consume <> l then
      Automaton.Checkpoint.Reject
    else
      arr.pops |>
      Word.to_letters |>
      List.fold_until  ~init: stk ~f: (fun stk pop ->
        match stk with
        | [] -> Stop Automaton.Checkpoint.Reject
        | fst :: rst ->
          if fst <> pop then
            Stop Reject
          else
            Continue rst
      ) ~finish: (fun stk ->
        let pshs =
          arr.pushes |>
          Word.to_letters |>
          List.rev in
        let stk2 = pshs @ stk
        and adv = if eps then 0 else 1 in
        Continue (stk2, adv)
      )

  let print arr =
    "[" ^
    Letter.print arr.consume ^
    "," ^
    Word.print_nonempty arr.pops ^
    "->" ^
    Word.print_nonempty arr.pushes ^
    "]"
end

include Automaton.Make(Memory)(Arrow)
