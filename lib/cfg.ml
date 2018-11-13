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

  let empty =
    { derivations = [];
    }

  let offset off var =
    { derivations =
        List.map var.derivations ~f: (fun dv ->
          match dv with
          | Derivation.Terminal _ -> dv
          | Derivation.NonTerminal ids ->
            Derivation.NonTerminal (List.map ids ~f: (fun id -> id + off))
        );
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

let new_empty () = ref
  { variables = [||];
  }

let get_var cfg var =
  cfg.variables |>
  Array.findi ~f: (fun _ var2 -> var = var2) |>
  Option.map ~f: (fun (id, _) -> id)

let add_var cfg var =
  let id = Array.length !cfg.variables in
  cfg := { variables = Array.append !cfg.variables [|var|]; };
  id

let have_var cfg var =
  match get_var !cfg var with
  | None -> add_var cfg var
  | Some id -> id

let add_vars cfg vars =
  cfg := { variables = Array.append !cfg.variables vars; }

let add_child_var par ch =
  let off = Array.length !par.variables in
  let vars = Array.map ch.variables ~f: (Var.offset off) in
  add_vars par vars;
  off

let add_derivation cfg id dv =
  let var = Array.get !cfg.variables id in
  let var = { Var.derivations = dv :: var.derivations; } in
  Array.set !cfg.variables id var

let parse vars =
  assert (not (List.is_empty vars));
  { variables = vars |> Util.validate_id_order |> Array.of_list;
  }

let print cfg =
  cfg.variables |>
  Array.to_list |>
  List.mapi ~f: Var.print |>
  String.concat ~sep: "\n"
