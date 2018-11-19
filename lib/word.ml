open Core

type t = String.t

let is_empty w =
  String.is_empty w || w = Letter.print Letter.epsilon

(** The index after the modifier for the letter starting at sidx.
    Assumes the letter has a modifier *)
let get_mod_end_idx w sidx =
  try
    let (e, _) =
      Util.String.findi w ~f: (fun idx c ->
        (idx <= sidx) || Char.is_digit c
      ) in e
  with
  | Caml.Not_found -> String.length w - sidx

let get w idx =
  let nidx = idx + 1 in
  { Letter.base_char = String.get w idx;
      int_mod =
        if (String.length w > nidx) && Char.is_digit (String.get w nidx) then
          let e = get_mod_end_idx w idx in
          Int.of_string (String.sub w ~pos: nidx ~len: (e - 1))
        else
          0;
  }

(** Returns epsilon if empty *)
let hd w =
  if String.is_empty w then
    Letter.epsilon
  else
    get w 0

(** Fails if empty *)
let tl_exn w =
  let w_len = String.length w in
  if (w_len > 1) && Char.is_digit (String.get w 1) then
    let e = get_mod_end_idx w 0 in
    String.sub w ~pos: e ~len: (w_len - e)
  else
    String.sub w ~pos: 1 ~len: (w_len - 1)

let rec to_letters w =
  let fst = hd w in
  if fst = Letter.epsilon then
    []
  else
    let rst = tl_exn w in
    fst :: to_letters rst

let length w =
  String.count w ~f: Char.is_alpha

let of_letter l =
  Letter.print l

let of_letters_no_epsilon ls =
  ls |>
  List.map ~f: Letter.print |>
  String.concat

let of_letters ls =
  ls |>
  List.filter ~f: (fun l -> l <> Letter.epsilon) |>
  of_letters_no_epsilon

let cons_no_epsilon l w =
  Letter.print l ^ w

let cons l w =
  if l = Letter.epsilon then
    w
  else
    cons_no_epsilon l w

let snoc w l =
  if l = Letter.epsilon then
    w
  else
    w ^ Letter.print l

let print w =
  w

(** Prints epsilon if empty *)
let print_nonempty w =
  if is_empty w then
    Letter.print Letter.epsilon
  else
    w

let gen =
  String.gen' Letter.char_gen
