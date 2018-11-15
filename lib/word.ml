open Core

type t = String.t

let is_empty w =
  String.is_empty w || w = Letter.print Letter.epsilon

let mod_hd_end w =
  try
    let (e, _) =
      Util.String.findi w ~f: (fun idx c ->
        (idx = 0) || Char.is_digit c
      ) in e
  with
  | Caml.Not_found -> String.length w

(** Returns epsilon if empty *)
let hd w =
  if String.is_empty w then
    Letter.epsilon
  else
    { Letter.base_char = String.get w 0;
      int_mod =
        if (String.length w > 1) && Char.is_digit (String.get w 1) then
          let e = mod_hd_end w in
          Int.of_string (String.sub w ~pos: 1 ~len: (e - 1))
        else
          0;
    }

(** Fails if empty *)
let tl_exn w =
  let w_len = String.length w in
  if (w_len > 1) && Char.is_digit (String.get w 1) then
    let e = mod_hd_end w in
    String.sub w ~pos: e ~len: (w_len - e)
  else
    String.sub w ~pos: 1 ~len: (w_len - 1)

let of_letter l =
  Letter.print l

let of_letters ls =
  ls |>
  List.map ~f: Letter.print |>
  String.concat

let print w =
  w

(** Prints epsilon if empty *)
let print_nonempty w =
  if is_empty w then
    Letter.print Letter.epsilon
  else
    w
