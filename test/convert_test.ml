open OUnit2
open Test_util

let assert_pda_to_spda ~pda ~spda =
  let act_spda = pda |>
    String.trim |>
    Lexing.from_string |>
    Lib.Parse.pda |>
    Lib.Convert.pda_to_spda |>
    Lib.Spda.print in
  assert_equal_str ~exp: (String.trim spda) ~act: act_spda

let assert_cfg_to_pda ~cfg ~pda =
  let act_pda = cfg |>
    String.trim |>
    Lexing.from_string |>
    Lib.Parse.cfg |>
    Lib.Convert.cfg_to_pda |>
    Lib.Pda.print in
  assert_equal_str ~exp: (String.trim pda) ~act: act_pda

let assert_pda_to_cfg ~pda ~cfg =
  let act_cfg = pda |>
    String.trim |>
    Lexing.from_string |>
    Lib.Parse.pda |>
    Lib.Convert.pda_to_spda |>
    Lib.Convert.spda_to_cfg |>
    Lib.Cfg.print in
  assert_equal_str ~exp: (String.trim cfg) ~act: act_cfg

let test_pda_to_spda _ =
  assert_pda_to_spda ~pda: {|
S  -[_,_->S]-> A
A@ -[a,S->_]-> A
   +[b,S->_]-> A
   +[_,S->ASA]-> A
   +[_,S->BSB]-> A
   +[a,A->_]-> A
   +[b,B->_]-> A
|} ~spda: {|
S  -[+S]-> A
A@ -[a]-> B
   +[b]-> C
   +[-S]-> D
   +[-S]-> G
   +[a]-> J
   +[b]-> K
B  -[-S]-> A
C  -[-S]-> A
D  -[+A]-> E
E  -[+S]-> F
F  -[+A]-> A
G  -[+B]-> H
H  -[+S]-> I
I  -[+B]-> A
J  -[-A]-> A
K  -[-B]-> A
|}

(* From https://prezi.com/zd6hscxa2wpz/cfg-to-pda/ *)
let test_cfg_to_pda _ =
  assert_cfg_to_pda ~cfg: {|
S -> a|b|ASA|BSB
A -> a
B -> b
|} ~pda: {|
S  -[_,_->S]-> A
A@ -[a,S->_]-> A
   +[b,S->_]-> A
   +[_,S->ASA]-> A
   +[_,S->BSB]-> A
   +[a,A->_]-> A
   +[b,B->_]-> A
|}

let test_pda_to_cfg _ =
  assert_pda_to_cfg ~pda: {|
S  -[_,_->S]-> A
A@ -[a,S->_]-> A
   +[b,S->_]-> A
   +[_,S->ASA]-> A
   +[_,S->BSB]-> A
   +[a,A->_]-> A
   +[b,B->_]-> A
|} ~cfg: {|
S  -[+S]-> A
A@ -[a]-> B
   +[b]-> C
   +[-S]-> D
   +[-S]-> G
   +[_,S->ASA]-> A
   +[_,S->BSB]-> A
   +[a]-> J
   +[b]-> K
B  -[-S]-> A
C  -[-S]-> A
D  -[+A]-> E
E  -[+S]-> F
F  -[+A]-> A
G  -[+B]-> H
H  -[+S]-> I
I  -[+B]-> A
J  -[-A]-> A
K  -[-B]-> A
|}

let suite =
  "parse_print" >:::
  [ "pda_to_spda" >:: test_pda_to_spda;
    "cfg_to_pda" >:: test_cfg_to_pda;
    "pda_to_cfg" >:: test_pda_to_cfg;
  ]
