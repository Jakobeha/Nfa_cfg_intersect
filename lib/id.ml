open Core
include Int

let alphabet =
  "SABCDEFGHIJKLMNOPQRTUVWXYZ"

let alphabet_len = String.length alphabet

let start =
  0

let base_char id =
  String.get alphabet (id mod alphabet_len)

let int_mod id =
  id / alphabet_len

let to_letter id =
  { Letter.base_char = base_char id;
    int_mod = int_mod id;
  }

let lex str =
  let str_len = String.length str in
  let base = String.index_exn alphabet (String.get str 0)
  and int_mod =
    if str_len = 1 then
      0
    else
      Int.of_string (String.sub str ~pos: 1 ~len: (str_len - 1)) in
  base + (int_mod * alphabet_len)

let print id =
  let im = int_mod id in
  Char.to_string (base_char id) ^
  ( if im = 0 then
      ""
    else
      Int.to_string im
  )
