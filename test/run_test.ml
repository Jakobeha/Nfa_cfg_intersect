open Core
open OUnit2
open Test_util

let assert_gen cfg exp_lvls =
  let cfg = Lib.Parse.cfg (Lexing.from_string (String.strip cfg)) in
  let act_lvls = Lib.Cfg.term_levels cfg in
  List.iteri exp_lvls ~f: (fun i exp_lvl ->
    try
      let act_lvl =
        Stream.next act_lvls |>
        List.map ~f: (fun trms ->
          trms |>
          List.map ~f: Lib.Cfg.Term.to_letter |>
          Lib.Word.of_letters
        ) in
      assert_equal exp_lvl act_lvl
        ~printer: (List.to_string ~f: Lib.Word.print)
        ~msg: (Printf.sprintf "Level %d" i)
    with
    | Stream.Failure ->
      assert_failure (
        Printf.sprintf "Level %d\nexpected: %s but got: nothing"
        i
        (List.to_string exp_lvl ~f: Lib.Word.print)
      )
  )

let assert_run f_parse f_run atm ~accepts ~rejects =
  let atm = f_parse (Lexing.from_string (String.strip atm)) in
  List.iter accepts ~f: (fun inp ->
    match f_run atm inp with
    | Lib.Run_result.Accept -> ()
    | Reject idx ->
      assert_failure (Printf.sprintf "Unexpectedly failed at %d in %S" idx inp)
  );
  List.iter rejects ~f: (fun inp ->
    match f_run atm inp with
    | Lib.Run_result.Accept ->
      assert_failure (Printf.sprintf "Unexpectedly succeeded in %S" inp)
    | Reject _ -> ()
  )

let test_cfg _ =
  assert_gen {|
S -> ASBSA|x
A -> a
B -> CBC|b
C -> c
|} [
  ["S"];
  ["ASBSA"; "x"];
  ["aASBSACBCASBSAa";"aASBSACBCxa";"aASBSAbASBSAa";"aASBSAbxa";"axCBCASBSAa";"axCBCxa";"axbASBSAa";"axbxa"];
];
  assert_run Lib.Parse.cfg Lib.Cfg.run {|
S -> ASB|x|_
A -> a
B -> a|b
|} ~accepts: [
  "";
  "x";
  "axa";
  "axb";
  "aaxba";
  "aaaababb";
] ~rejects: [
  "a";
  "ax";
  "bxa";
  "aab"
]

let test_pda _ =
  assert_run Lib.Parse.pda Lib.Pda.run {|
S  -[a,_->a]-> S
   +[c,_->_]-> A
   +[d,_->a]-> A
A  -[b,a->_]-> A
   +[_,_->_]-> B
B@ -*
|} ~accepts: [
  "c";
  "aacbb";
  "adbb";
] ~rejects: [
  "";
  "ac";
  "aac";
  "adb";
  "ce"
]

let test_spda _ =
  assert_run Lib.Parse.spda Lib.Spda.run {|
S  -[a]-> C
   +[c]-> A
   +[d]-> D
A  -[b]-> E
   +[_]-> B
   +[-e]-> B
B@ -*
C  -[+a]-> S
D  -[+a]-> A
E  -[-a]-> A
|} ~accepts: [
  "c";
  "acb";
  "aacbb";
  "adbb";
] ~rejects: [
  "";
  "ac";
  "aac";
  "adb";
  "ce"
]

let test_nfa _ =
  assert_run Lib.Parse.nfa Lib.Nfa.run {|
S  -a-> S
   +b-> S
   +_-> A
A  -a-> A
   +b-> B
B@ -b-> B
|} ~accepts: [
  "babababaabbb";
  "babababab";
  "aabbbaabbab";
] ~rejects: [
  "";
  "aaa";
  "aabba";
  "aacb";
]

let suite =
  "run" >:::
  [ "cfg" >:: test_cfg;
    "pda" >:: test_pda;
    "spda" >:: test_spda;
    "nfa" >:: test_nfa;
  ]
