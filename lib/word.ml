open Core
include String

let is_empty w =
  String.is_empty w || w = Letter.to_string Letter.epsilon

(** Returns epsilon if empty *)
let hd w =
  if String.is_empty w then
    Letter.epsilon
  else
    String.get w 0

(** Fails if empty *)
let tl_exn w =
  String.sub w ~pos: 1 ~len: (String.length w - 1)

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
