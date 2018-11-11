open Core
include String

let of_letters ls =
  String.of_char_list ls

let print w =
  w

(** Prints epsilon if empty *)
let print_nonempty w =
  if is_empty w then
    Letter.print Letter.epsilon
  else
    w
