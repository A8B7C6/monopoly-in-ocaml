open OUnit2
open Monopoly
open Locations
open Player

(* (** [sample_test name input expected_output] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with [input]. *) let
   index_test (name : string) (input : string) (expected_output : string) : test
   = name >:: fun _ -> assert_equal input expected_output

   (** [sample_test name input expected_output] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with [input]. *) let
   initialize_tiles_test (name : string) (json : Yojson.Basic.t)
   (expected_output : (int * tile_type) list) : test = name >:: fun _ ->
   assert_equal json expected_output *)
let mono = Yojson.Basic.from_file "Monopoly.json"

let tiles_list_test name json expected : test =
  name >:: fun _ -> assert_equal expected (tiles_list json)

let locations_tests = [ (*tiles_list_test "full monopoly board" mono () *) ]
let board_tests = []

let suite =
  "Monopoly Test Suite: " >::: List.flatten [ locations_tests; board_tests ]

let _ = run_test_tt_main suite
