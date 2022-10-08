open OUnit2

(** [sample_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [input]. *)
let index_test (name : string) (input : string) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal input expected_output

let board_tests = [ index_test "Simple equality test: " "hi" "hi" ]
let suite = "Monopoly Test Suite: " >::: List.flatten [ board_tests ]
let _ = run_test_tt_main suite
