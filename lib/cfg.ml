open Core

module Derivation = struct
  type t =
    | Terminal of Letter.t
    | NonTerminal of Id.t * Id.t

  let print d =
    match d with
    | Terminal l -> Letter.print l
    | NonTerminal (v1, v2) -> Id.print v1 ^ Id.print v2

end

module Var = struct
  type t =
    { derivations : Derivation.t List.t;
    }

  let parse dvs =
    { derivations = List.map ~f: (fun dv ->
        match dv with
        | Derivation.Terminal t ->
          assert (t <> Letter.epsilon);
          Derivation.Terminal t
        | NonTerminal (v1, v2) ->
          assert (v1 <> Id.start && v2 <> Id.start);
          NonTerminal (v1, v2)
      ) dvs;
    }

  let print_start var al_emp =
    "S -> " ^
    String.concat ~sep: "|" (
      List.map ~f: Derivation.print var.derivations @
      (if al_emp then [Char.to_string Letter.epsilon] else [])
    )

  let print_reg idx var =
    Id.print (idx + 1) ^
    " -> " ^
    String.concat ~sep: "|" (
      List.map ~f: Derivation.print var.derivations
    )
end

(** In chomksy normal form *)
type t =
  { variables : Var.t Array.t;
    allow_empty : Bool.t;
  }

let to_pda cfg =
  let pda = Pda.new_empty () in
  (* Initial state *)
  Pda.add_empty_state pda false;
  (* Derivations state *)
  Pda.add_empty_state pda false;
  (* Final state *)
  Pda.add_empty_state pda true;
  let start_id = 0
  and mid_id = 1
  and end_id = 2 in
  (* Push the start state onto the stack *)
  Pda.add_transition pda (
    start_id,
    { Pda.Arrow.consume = Letter.epsilon; pops = ""; pushes = "$S"; },
    mid_id
  );
  Pda.add_transition pda (
    mid_id,
    { Pda.Arrow.consume = Letter.epsilon; pops = "$"; pushes = ""; },
    end_id
  );
  (* For every possible derivation, add a transition where the PDA
     "chooses" the derivation and follows it. *)
  Array.iteri ~f: (fun idx var ->
    List.iter ~f: (fun dv ->
      let arr =
        match dv with
        | Cfg.Derivation.Terminal l ->
          { Pda.Arrow.consume = l;
            pops = Letter.to_string (Id.to_letter idx);
            pushes = "";
          }
        | NonTerminal (v1, v2) ->
          { Pda.Arrow.consume = Letter.epsilon;
            pops = Letter.to_string (Id.to_letter idx);
            pushes = String.of_char_list [Id.to_letter v2, Id.to_letter v1];
          } in
      Pda.add_transition pda (mid_id, arr, mid_id)
    ) var.derivations
  ) cfg.variables;
  (* Generate a state for every variable *)
  !pda

let parse vars =
  assert (not (List.is_empty vars));
  let vars = Util.validate_id_order vars in
  match vars with
  | [] -> assert false
  | dvs1 :: dvss ->
    let a_emp = Option.is_some (List.find ~f: (fun dev -> dev = Derivation.Terminal Letter.epsilon) dvs1)
    and var1 = dvs1 |>
      List.filter ~f: (fun dev -> dev <> Derivation.Terminal Letter.epsilon) |>
      Var.parse
    and vars = List.map ~f: Var.parse dvss in
    { variables = Array.of_list (var1 :: vars);
      allow_empty = a_emp;
    }

let print cfg =
  let vars = Array.to_list cfg.variables in
  String.concat ~sep: "\n" (
    Var.print_start (List.hd_exn vars) cfg.allow_empty ::
    List.mapi ~f: Var.print_reg (List.tl_exn vars)
  )
