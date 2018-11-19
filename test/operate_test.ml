open Core
open OUnit2
open Test_util

let assert_run_intersect ?trials (x, frx, nx) (y, fry, ny) (r, frr) =
  Quickcheck.test ?trials Lib.Word.gen ~f: (fun w ->
    let rx = frx x w
    and ry = fry y w
    and rr = frr r w in
    match (rx, ry, rr) with
    | (Lib.Run_result.Accept, Lib.Run_result.Accept, Lib.Run_result.Accept)
    | (Accept, Reject _, Reject _)
    | (Reject _, Accept, Reject _)
    | (Reject _, Reject _, Reject _) -> ()
    | (Accept, Reject idx, Accept) ->
      assert_failure (
        Printf.sprintf
        "%s accepted and %s rejected (at %d), but intersection accepted:\n%S"
        nx
        ny
        idx
        w
      )
    | (Reject idx, Accept, Accept) ->
      assert_failure (
        Printf.sprintf
        "%s rejected (at %d) and %s accepted, but intersection accepted:\n%S"
        nx
        idx
        ny
        w
      )
    | (Reject xidx, Reject yidx, Accept) ->
      assert_failure (
        Printf.sprintf
        "%s rejected (at %d) and %s rejected (at %d), but intersection accepted:\n%S"
        nx
        xidx
        ny
        yidx
        w
      )
    | (Accept, Accept, Reject idx) ->
      assert_failure (
        Printf.sprintf
        "%s accepted and %s accepted, but intersection rejected (at %d):\n%S"
        nx
        ny
        idx
        w
      )
  )

let assert_intersect_spda_nfa ?trials ~spda ~nfa ~exp =
  let spda = spda |> String.strip |> Lexing.from_string |> Lib.Parse.spda
  and nfa = nfa |> String.strip |> Lexing.from_string |> Lib.Parse.nfa in
  let act = Lib.Operate.intersect_spda_nfa spda nfa in
  assert_equal_str
    ~exp: (String.strip exp)
    ~act: (Lib.Spda.print act);
  assert_run_intersect
    ?trials
    (spda, Lib.Spda.run, "spda")
    (nfa, Lib.Nfa.run, "nfa")
    (act, Lib.Spda.run)

let assert_intersect_cfg_nfa ?trials ~cfg ~nfa ~run =
  let cfg = cfg |> String.strip |> Lexing.from_string |> Lib.Parse.cfg
  and nfa = nfa |> String.strip |> Lexing.from_string |> Lib.Parse.nfa in
  let act = Lib.Operate.intersect_cfg_nfa cfg nfa in
  if run then
    assert_run_intersect
      ?trials
      (cfg, Lib.Cfg.run, "cfg")
      (nfa, Lib.Nfa.run, "nfa")
      (act, Lib.Cfg.run)

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
|} ?trials: None

let test_intersect_cfg_nfa _ =
  assert_intersect_cfg_nfa ~cfg: {|
S -> ASA|b
A -> a
|} ~nfa: {|
S  -a-> A
A@ -a-> A
   +b-> A
|} ~trials: 2 ~run: true;
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
|} ~trials: 2 ~run: false

let suite =
  "operate" >:::
  [ "intersect_spda_nfa" >:: test_intersect_spda_nfa;
    "intersect_cfg_nfa" >:: test_intersect_cfg_nfa;
  ]
