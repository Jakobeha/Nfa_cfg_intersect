open Core
include Int

(** Assumes no more than 26 variables *)
let alphabet =
  "SABCDEFGHIJKLMNOPQRTUVWXYZ"

let start =
  0

let to_char id =
  String.get alphabet id

let to_letter id =
  to_char id

let lex chr =
  String.index_exn alphabet chr

let print id =
  Char.to_string (to_char id)
