open Core
open OUnit2

let assert_equal_str ~exp ~act =
  assert_equal ~printer: (fun x -> x) (String.strip exp) act

let assert_run_equal ?trials (x, frx, nx) (y, fry, ny) =
  Quickcheck.iter Lib.Word.gen ?trials ~f: (fun w ->
    let rx = frx x w
    and ry = fry y w in
    match (rx, ry) with
    | (Lib.Run_result.Accept, Lib.Run_result.Accept)
    | (Reject _, Reject _) -> ()
    | (Accept, Reject idx) ->
      assert_failure (
        Printf.sprintf "%s accepted but %s rejected (at %d):\n%S"
        nx
        ny
        idx
        w
      )
    | (Reject idx, Accept) ->
      assert_failure (
        Printf.sprintf "%s rejected (at %d) but %s accepted:\n%S"
        nx
        idx
        ny
        w
      )
  )

let assert_run_equal_cfg_pda cfg pda ~max_level ~try_gen =
  let cfg_words =
    Lib.Cfg.word_levels cfg |>
    Stream.npeek max_level |>
    List.concat in
  List.iter cfg_words ~f: (fun cfg_word ->
    match Lib.Pda.run pda cfg_word with
    | Lib.Run_result.Accept -> ()
    | Reject idx ->
      assert_failure (
        Printf.sprintf "PDA failed at %d on cfg word %S"
        idx
        cfg_word
      )
  );
  if try_gen then
    assert_run_equal
      ~trials: 15
      (cfg, Lib.Cfg.run, "cfg")
      (pda, Lib.Pda.run, "pda")
