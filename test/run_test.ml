open Core
open OUnit2
open Test_util

let assert_gen ?(vars=true) cfg exp_lvls =
  let cfg = Lib.Parse.cfg (Lexing.from_string (String.strip cfg)) in
  let act_lvls = Lib.Cfg.term_levels cfg in
  List.iteri exp_lvls ~f: (fun i exp_lvl ->
    try
      let act_lvl = Stream.next act_lvls in
      let act_lvl =
        if vars then
          act_lvl |>
          List.map ~f: (fun trms ->
            trms |>
            List.map ~f: Lib.Cfg.Term.to_letter |>
            Lib.Word.of_letters
          )
        else
          Lib.Cfg.Term.words_in_level act_lvl in
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
];
  assert_gen ~vars: false {|
S -> b|AM|BT|CZ|DE1|EK1|FA|HU|IS1|JF1|KL1|RS|UM|VZ|WE1|XK1|YE|ZK|S1Q|A1X|B1I1
A -> SG|BU|CS1|DF1|EL1|RA|TG|VS1|WF1|XL1
B -> _|SH|SJ|AN|AP|BW|CA1|CB1|DG1|EM1|EO1|RD|TJ|UP|VB1|XO1
C -> SI|AO|BV|DH1|EN1|RC|TI|UO|WH1|XN1
D -> SJ|AP|BW|CB1|EO1|RD|TJ|UP|VB1|XO1
E -> SK|AQ|BX|CC1|DI1|RE|TK|UQ|VC1|WI1
F -> GL|HR|IY|JD1|KJ1
G -> b|FA|HU|IS1|JF1|KL1
H -> FB|FD|GL|GN|GP|HR|HW|IY|IA1|IB1|JD1|JG1|KJ1|KM1|KO1
I -> FC|GO|HV|JH1|KN1
J -> FD|GP|HW|IB1|KO1
K -> a|FE|GQ|HX|IC1|JI1
L -> MF|NR|OY|PD1|QJ1
M -> LS|NT|OZ|PE1|QK1
N -> LB|LD|MF|MH|MJ|NR|NW|OY|OA1|OB1|PD1|PG1|QJ1|QM1|QO1
O -> LC|MI|NV|PH1|QN1
P -> LD|MJ|NW|OB1|QO1
Q -> LE|MK|NX|OC1|PI1
R -> TF|UL|VY|WD1|XJ1
T -> RS|UM|VZ|WE1|XK1|YE|ZK|S1Q|A1X|B1I1
U -> RA|TG|VS1|WF1|XL1
V -> RC|TI|UO|WH1|XN1
W -> RD|TJ|UP|VB1|XO1
X -> RE|TK|UQ|VC1|WI1
Y -> ZF|S1L|A1R|B1D1|C1J1
Z -> RE|TK|UQ|VC1|WI1|YS|S1M|A1T|B1E1|C1K1|D1E|E1K|F1Q|G1X|H1C1
S1 -> YA|ZG|A1U|B1F1|C1L1
A1 -> YB|YD|ZF|ZH|ZJ|S1L|S1N|S1P|A1R|A1W|B1D1|B1G1|C1J1|C1M1|C1O1
B1 -> YD|ZJ|S1P|A1W|C1O1
C1 -> YE|ZK|S1Q|A1X|B1I1
D1 -> TF|UL|VY|WD1|XJ1|E1F|F1L|G1R|H1Y|I1J1
E1 -> b|FA|HU|IS1|JF1|KL1|RS|UM|VZ|WE1|XK1|YE|ZK|S1Q|A1X|B1I1|D1S|F1M|G1T|H1Z|I1K1
F1 -> RA|TG|VS1|WF1|XL1|D1A|E1G|G1U|H1S1|I1L1
G1 -> _|TF|UL|VY|WG1|XJ1|D1B|E1F|E1H|F1L|F1N|G1R|H1Y|H1A1|I1J1|I1M1
H1 -> RC|TI|UO|WH1|XN1|D1C|E1I|F1O|G1V|I1N1
I1 -> RE|TK|UQ|VC1|WI1|D1E|E1K|F1Q|G1X|H1C1
J1 -> K1F|L1L|M1R|N1Y|O1D1
K1 -> J1S|L1M|M1T|N1Z|O1E1
L1 -> J1A|K1G|M1U|N1S1|O1F1
M1 -> J1B|J1D|K1F|K1H|K1J|L1L|L1N|L1P|M1R|M1W|N1Y|N1A1|N1B1|O1D1|O1G1
N1 -> J1C|K1I|L1O|M1V|O1H1
O1 -> J1D|K1J|L1P|M1W|N1B
|} [
  [];
  ["b"];
  [];
  ["baa"];
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
  [ "cfg" >: test_case ~length: Long test_cfg;
    "pda" >:: test_pda;
    "spda" >:: test_spda;
    "nfa" >:: test_nfa;
  ]
