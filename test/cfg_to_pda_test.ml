open OUnit2
open Test_util

let assert_cfg_to_pda ~cfg ~pda =
  let act_pda = cfg |>
    String.trim |>
    Lexing.from_string |>
    Lib.Parse.cfg |>
    Lib.Convert.cfg_to_pda |>
    Lib.Pda.print in
  assert_equal_str ~exp: (String.trim pda) ~act: act_pda

(* From https://prezi.com/zd6hscxa2wpz/cfg-to-pda/ *)
let test_palindrome _ =
  assert_cfg_to_pda ~cfg: {|
S -> a|b|ASA|BSB
A -> a
B -> b
|} ~pda: {|
S  -[0,0->S]-> A
A@ -[a,S->0]-> A
   +[b,S->0]-> A
   +[0,S->ASA]-> A
   +[0,S->BSB]-> A
   +[a,A->0]-> A
   +[b,B->0]-> A
|}

let suite =
  "parse_print" >:::
  [ "palindrome" >:: test_palindrome;
  ]
