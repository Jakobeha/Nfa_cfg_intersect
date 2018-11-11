(** Context free grammar.
    Not necessarily in Chomsky Normal Form, but limited in that
    derivations must contain 1 terminal (can be epsilon)
    or a list of variables (empty list = epsilon terminal). *)

open Core

module Derivation = struct
  type t =
    | Terminal of Letter.t
    | NonTerminal of Id.t List.t

  let print d =
    match d with
    | Terminal l -> Letter.print l
    | NonTerminal vs -> String.concat (List.map ~f: Id.print vs)

end

module Var = struct
  type t =
    { derivations : Derivation.t List.t;
    }

  let print idx var =
    Id.print idx ^
    " -> " ^
    String.concat ~sep: "|" (
      List.map ~f: Derivation.print var.derivations
    )
end

type t =
  { variables : Var.t Array.t;
  }

let to_pda cfg =
  let pda = Pda.new_empty () in
  (* Initial state *)
  Pda.add_empty_state pda false;
  (* Derivations state *)
  Pda.add_empty_state pda true;
  let start_id = 0
  and mid_id = 1 in
  (* Push the start state onto the stack *)
  Pda.add_transition pda (
    start_id,
    { Pda.Arrow.consume = Letter.epsilon; pops = ""; pushes = "S"; },
    mid_id
  );
  (* For every possible derivation, add a transition where the PDA
     "chooses" the derivation and follows it. *)
  Array.iteri ~f: (fun idx var ->
    List.iter ~f: (fun dv ->
      let arr =
        match dv with
        | Derivation.Terminal l ->
          { Pda.Arrow.consume = l;
            pops = Letter.to_string (Id.to_letter idx);
            pushes = "";
          }
        | NonTerminal vs ->
          { Pda.Arrow.consume = Letter.epsilon;
            pops = Letter.to_string (Id.to_letter idx);
            pushes =
              vs |>
              List.rev_map ~f: Id.to_letter |>
              Word.of_letters;
          } in
      Pda.add_transition pda (mid_id, arr, mid_id)
    ) var.derivations
  ) cfg.variables;
  (* Generate a state for every variable *)
  !pda

let parse vars =
  assert (not (List.is_empty vars));
  { variables = vars |> Util.validate_id_order |> Array.of_list;
  }

let print cfg =
  cfg.variables |>
  Array.to_list |>
  List.mapi ~f: Var.print |>
  String.concat ~sep: "\n"
