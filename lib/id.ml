open Core
include Int

(** Assumes no more than 26 variables *)
let alphabet =
  "SABCDEFGHIJKLMNOPQRTUVWXYZ"

let start =
  0

let lex chr =
  String.index_exn alphabet chr

let print id =
  Char.to_string (String.get alphabet id)
