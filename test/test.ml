open OUnit2

let suite =
  test_list
    [ Parse_print_test.suite;
      (* Intersect_test.suite; *)
    ]

let _ =
  run_test_tt_main suite
