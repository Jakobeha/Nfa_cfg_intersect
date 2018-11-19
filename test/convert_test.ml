open Core
open OUnit2
open Test_util

let assert_pda_to_spda ~pda ~spda =
  let pda =
    pda |>
    String.strip |>
    Lexing.from_string |>
    Lib.Parse.pda in
  let act_spda = Lib.Convert.pda_to_spda pda in
  assert_equal_str
    ~exp: (String.strip spda)
    ~act: (Lib.Spda.print act_spda);
  assert_run_equal
    (pda, Lib.Pda.run, "pda")
    (act_spda, Lib.Spda.run, "spda")

let assert_cfg_to_pda ~cfg ?pda ~max_level ~try_gen =
  let cfg =
    cfg |>
    String.strip |>
    Lexing.from_string |>
    Lib.Parse.cfg in
  let act_pda = Lib.Convert.cfg_to_pda cfg in
  ( match pda with
    | None -> ()
    | Some pda ->
      assert_equal_str
        ~exp: (String.strip pda)
        ~act: (Lib.Pda.print act_pda);
  );
  assert_run_equal_cfg_pda cfg act_pda ~max_level ~try_gen

let assert_pda_to_cfg ~pda ?cfg_orig ?cfg_optim ~max_level ~try_gen =
  let pda =
    pda |>
    String.strip |>
    Lexing.from_string |>
    Lib.Parse.pda in
  let act_cfg =
    pda |>
    Lib.Convert.pda_to_spda |>
    Lib.Convert.spda_to_cfg |>
    ref in
  ( match cfg_orig with
    | None -> ()
    | Some cfg ->
      assert_equal_str
        ~exp: (String.strip cfg)
        ~act: (Lib.Cfg.print !act_cfg)
  );
  ( match cfg_optim with
    | None -> ()
    | Some cfg ->
      Lib.Cfg.optimize act_cfg;
      assert_equal_str
        ~exp: (String.strip cfg)
        ~act: (Lib.Cfg.print !act_cfg)
  );
  assert_run_equal_cfg_pda !act_cfg pda ~max_level ~try_gen

let test_pda_to_spda _ =
  assert_pda_to_spda ~pda: {|
S  -[a,_->a]-> S
   +[c,_->_]-> A
   +[d,_->a]-> A
A  -[b,a->_]-> A
   +[_,_->_]-> B
B@ -*
|} ~spda: {|
S  -[c]-> A
   +[a]-> C
   +[d]-> D
A  -[_]-> B
   +[b]-> E
B@ -*
C  -[+a]-> S
D  -[+a]-> A
E  -[-a]-> A
|};
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
|} ~max_level: 10 ~try_gen: true

let test_pda_to_cfg _ =
  assert_pda_to_cfg ~pda: {|
S  -[a,_->_]-> A
A@ -[b,_->_]-> A
|} ~cfg_orig: {|
S -> B
A -> _
B -> a|AB|BD
C -> CA|DC
D -> b|_
|} ~cfg_optim: {|
S -> a|SA
A -> _|b
|} ~max_level: 10 ~try_gen: true;
  assert_pda_to_cfg ~pda: {|
S  -[_,_->S]-> A
A@ -[a,S->_]-> A
   +[_,S->SS]-> A
|} ~cfg_orig: {|
S -> B
A -> _
B -> H|AB|BG|CL|DQ|EW
C -> AC|BH|CM|DR|EX
D -> G|AD|BI|CN|DT|EY
E -> AE|BJ|CO|DU|EZ
F -> FA|GF|HK|IP|JV
G -> _
H -> a|FC|GH|HM|IR|JX
I -> FD|GI|HN|IT|JY
J -> FE|GJ|HO|IU|JZ
K -> KA|LF|MK|NP|OV
L -> KB|LG|ML|NQ|OW
M -> _
N -> KD|LI|MN|NT|OY
O -> KE|LJ|MO|NU|OZ
P -> PA|QF|RK|TP|UV
Q -> X|PB|QG|RL|TQ|UW
R -> PC|QH|RM|TR|UX
T -> W|_
U -> PE|QJ|RO|TU|UZ
V -> VA|WF|XK|YP|ZV
W -> H|VB|WG|XL|YQ|ZW
X -> VC|WH|XM|YR|ZX
Y -> G|VD|WI|XN|YT|ZY
Z -> _
|} ?cfg_optim: None ~max_level: 1 ~try_gen: false;
  assert_pda_to_cfg ~pda: {|
S  -[_,_->S]-> A
A@ -[b,S->_]-> A
   +[_,S->AAS]-> A
   +[a,A->_]-> A
|} ?cfg_orig: None ?cfg_optim: None ~max_level: 1 ~try_gen: false;
  assert_pda_to_cfg ~pda: {|
S  -[_,_->S]-> A
A@ -[b,S->_]-> A
   +[_,S->ASA]-> A
   +[a,A->_]-> A
|} ?cfg_orig: None ?cfg_optim: None ~max_level: 1 ~try_gen: false

let suite =
  "convert" >:::
  [ "pda_to_spda" >:: test_pda_to_spda;
    "cfg_to_pda" >:: test_cfg_to_pda;
    "pda_to_cfg" >:: test_pda_to_cfg;
  ]
