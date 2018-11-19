open Core

type t =
  { base_char : Char.t;
    int_mod : Int.t;
  }

let epsilon =
  { base_char = Char.of_string "_";
    int_mod = 0;
  }

let lex str =
  let str_len = String.length str in
  { base_char = String.get str 0;
    int_mod =
      if str_len = 1 then
        0
      else
        Int.of_string (String.sub str ~pos: 1 ~len: (str_len - 1));
  }

let print l =
  Char.to_string l.base_char ^
  ( if l.int_mod = 0 then
      ""
    else
      Int.to_string l.int_mod
  )

let char_gen =
  Quickcheck.Generator.(union [
    singleton 'a';
    singleton 'b';
    singleton 'c';
    singleton 'd';
  ])
