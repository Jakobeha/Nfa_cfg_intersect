open OUnit2
open Test_util

let assert_cfg_to_pda ~cfg ~pda =
  let act_pda = cfg |>
    Lexing.from_string |>
    Lib.Parse.cfg |>
    Lib.Cfg.to_pda |>
    Lib.Pda.print in
  assert_equal_str ~exp: pda ~act: act_pda

(* From https://prezi.com/zd6hscxa2wpz/cfg-to-pda/ *)
let test_palindrome _ =
  assert_cfg_to_pda ~cfg: (*
S -> a|b
S -> aSa|bSb
*) {|
S -> a|AB|b|DE
A -> a|BC|b|DE
B -> a
C -> AB
D -> b
E -> AD
|} ~pda: {|
S  -[0,0->s]-> A
A  -[0,0->z]-> B
B  -[0,s->a]-> B
   +[0,s->b]-> B
   +[0,s->a]-> C
   +[0,s->b]-> E
   +[a,a->0]-> B
   +[b,b->0]-> B
   +[0,z->0]-> G
C  -[0,0->s]-> D
D  -[0,0->a]-> B
E  -[0,0->s]-> F
F  -[0,0->b]-> G
G@ -*
|}

let suite =
  "parse_print" >:::
  [ "palindrome" >:: test_palindrome;
  ]
