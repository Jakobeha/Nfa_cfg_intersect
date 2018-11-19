open OUnit2
open Test_util

let assert_parse_print f_parse f_print inp =
  assert_equal_str ~exp: inp ~act: (f_print (f_parse (Lexing.from_string (String.trim inp))))

let test_cfg _ =
  assert_parse_print Lib.Parse.cfg Lib.Cfg.print {|
S -> AB|BC|DE|_
A -> AB|a
B -> BC|DE
C -> c
D -> AC|d
E -> f
|}

let test_pda _ =
  assert_parse_print Lib.Parse.pda Lib.Pda.print {|
S  -[a,_->a]-> S
   +[c,_->_]-> A
   +[d,_->a]-> A
A  -[b,a->_]-> A
   +[_,_->_]-> B
B@ -*
|}

let test_spda _ =
  assert_parse_print Lib.Parse.spda Lib.Spda.print {|
S  -[-a]-> S
   +[+c]-> A
   +[d]-> A
   +[-e]-> B
A  -[_]-> A
   +[-B]-> B
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
    "spda" >:: test_spda;
    "nfa" >:: test_nfa;
  ]
