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
