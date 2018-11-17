open OUnit2
open Test_util

let assert_intersect_spda_nfa ~spda ~nfa ~exp =
  let spda = spda |> String.trim |> Lexing.from_string |> Lib.Parse.spda
  and nfa = nfa |> String.trim |> Lexing.from_string |> Lib.Parse.nfa in
  let act = Lib.Operate.intersect_spda_nfa spda nfa in
  assert_equal_str
    ~exp: (String.trim exp)
    ~act: (Lib.Spda.print act)

let assert_intersect_cfg_nfa ~cfg ~nfa ~exp =
  let cfg = cfg |> String.trim |> Lexing.from_string |> Lib.Parse.cfg
  and nfa = nfa |> String.trim |> Lexing.from_string |> Lib.Parse.nfa in
  let act = Lib.Operate.intersect_cfg_nfa cfg nfa in
  assert_equal_str
    ~exp: (String.trim exp)
    ~act: (Lib.Cfg.print act)

let test_intersect_spda_nfa _ =
  assert_intersect_spda_nfa ~spda: {|
S  -[a]-> B
   +[b]-> C
   +[c]-> S
A@ -[b]-> C
   +[c]-> A
B  -[+x]-> S
C  -[-x]-> A
|} ~nfa: {|
S  -_-> A
   +_-> B
A@ -a-> B
   +b-> B
B@ -c-> A
|} ~exp: {|
S  -[_]-> A
   +[_]-> B
A  -[a]-> H
   +[b]-> K
B  -[c]-> A
C  -[_]-> D
   +[_]-> E
D@ -[b]-> K
E@ -[c]-> D
F  -[+x]-> S
   +[_]-> G
   +[_]-> H
G  -[+x]-> A
H  -[+x]-> B
I  -[-x]-> C
   +[_]-> J
   +[_]-> K
J  -[-x]-> D
K  -[-x]-> E
|}

let test_intersect_cfg_nfa _ =
  assert_intersect_cfg_nfa ~cfg: {|
S -> ASB|_
A -> a|CA|AC
B -> b|CB|BC
C -> c
|} ~nfa: {|
S  -_-> A
   +_-> B
A@ -a-> B
   +b-> B
B@ -c-> A
|} ~exp: {|
...
|}

let suite =
  "parse_print" >:::
  [ "intersect_spda_nfa" >:: test_intersect_spda_nfa;
    "intersect_cfg_nfa" >:: test_intersect_cfg_nfa;
  ]
