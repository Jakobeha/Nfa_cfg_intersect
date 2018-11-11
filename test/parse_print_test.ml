open OUnit2
open Test_util

let assert_parse_print f_parse f_print inp =
  assert_equal_str ~exp: inp ~act: (f_print (f_parse (Lexing.from_string (String.trim inp))))

let test_cfg _ =
  assert_parse_print Lib.Parse.cfg Lib.Cfg.print {|
S -> AB|BC|DE|0
A -> AB|a
B -> BC|DE
C -> c
D -> AC|d
E -> f
|}

let test_pda _ =
  assert_parse_print Lib.Parse.pda Lib.Pda.print {|
S  -[a,0->a]-> S
   +[c,0->0]-> A
   +[d,0->a]-> A
A  -[b,a->0]-> A
   +[0,0->0]-> B
B@ -*
|}

let test_nfa _ =
  assert_parse_print Lib.Parse.nfa Lib.Nfa.print {|
S  -a-> S
A  -*
|}

let suite =
  "parse_print" >:::
  [ "cfg" >:: test_cfg;
    "pda" >:: test_pda;
    "nfa" >:: test_nfa;
  ]
