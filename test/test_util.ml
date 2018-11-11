open OUnit2

let assert_equal_str ~exp ~act =
  assert_equal ~printer: (fun x -> x) (String.trim exp) act
