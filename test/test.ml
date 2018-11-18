open OUnit2

let suite =
  test_list
    [ Parse_print_test.suite;
      Run_test.suite;
      Convert_test.suite;
      Operate_test.suite;
    ]

let _ =
  run_test_tt_main suite
