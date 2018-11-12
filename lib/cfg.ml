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

let new_empty () = ref
  { variables = [||];
  }

let parse vars =
  assert (not (List.is_empty vars));
  { variables = vars |> Util.validate_id_order |> Array.of_list;
  }

let print cfg =
  cfg.variables |>
  Array.to_list |>
  List.mapi ~f: Var.print |>
  String.concat ~sep: "\n"
