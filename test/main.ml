open OUnit2
open Monopoly
open Locations
(*open Player*)

(*let rec print_tile_list (list : (int * tile_type) list) = match list with | []
  -> () | (a, Property {name; price}) :: rest -> Printf.printf "%i, %s; " a
  Property { name; price }; print_tile_list rest | (a, b) :: rest ->
  Printf.printf "%i, %s; " a b; print_tile_list rest*)

(* (** [sample_test name input expected_output] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with [input]. *) let
   index_test (name : string) (input : string) (expected_output : string) : test
   = name >:: fun _ -> assert_equal input expected_output

   (** [sample_test name input expected_output] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with [input]. *) let
   initialize_tiles_test (name : string) (json : Yojson.Basic.t)
   (expected_output : (int * tile_type) list) : test = name >:: fun _ ->
   assert_equal json expected_output *)
let data_dir_prefix = "data" ^ Filename.dir_sep
let mono = Yojson.Basic.from_file (data_dir_prefix ^ "Monopoly.json")

let tiles_list_test name json expected : test =
  name >:: fun _ -> assert_equal expected (tiles_list json)

let locations_tests =
  [
    tiles_list_test "full monopoly board" mono
      [
        make_tile 0 "go" "" 0;
        make_tile 1 "property" "Mediterranean Avenue" 60;
        make_tile 2 "cc" "" 0;
        make_tile 3 "property" "Baltic Avenue" 60;
        make_tile 4 "property" "Income Tax" 200;
        make_tile 5 "property" "Reading Railroad" 200;
        make_tile 6 "property" "Oriental Avenue" 100;
        make_tile 7 "chance" "" 0;
        make_tile 8 "property" "Vermont Avenue" 100;
        make_tile 9 "property" "Connecticut Avenue" 120;
        make_tile 10 "go" "" 0;
        make_tile 11 "property" "St. Charles Place" 140;
        make_tile 12 "property" "Electric Company" 150;
        make_tile 13 "property" "States Avenue" 140;
        make_tile 14 "property" "Virginia Avenue" 160;
        make_tile 15 "property" "Pennsylvania Railroad" 200;
        make_tile 16 "property" "St. James Place" 180;
        make_tile 17 "cc" "" 0;
        make_tile 18 "property" "Tennessee Avenue" 180;
        make_tile 19 "property" "New York Avenue" 200;
        make_tile 20 "go" "" 0;
        make_tile 21 "property" "Kentucky Avenue" 220;
        make_tile 22 "chance" "" 0;
        make_tile 23 "property" "Indiana Avenue" 220;
        make_tile 24 "property" "Illinois Avenue" 240;
        make_tile 25 "property" "B & O Railroad" 200;
        make_tile 26 "property" "Atlantic Avenue" 260;
        make_tile 27 "property" "Ventnor Avenue" 260;
        make_tile 28 "property" "Water Works" 150;
        make_tile 29 "property" "Marvin Gardens" 280;
        make_tile 30 "jail" "" 0;
        make_tile 31 "property" "Pacific Avenue" 300;
        make_tile 32 "property" "North Carolina Avenue" 300;
        make_tile 33 "cc" "" 0;
        make_tile 34 "property" "Pennsylvania Avenue" 320;
        make_tile 35 "property" "Short Line" 200;
        make_tile 36 "chance" "" 0;
        make_tile 37 "property" "Park Place" 350;
        make_tile 38 "property" "Luxury Tax" 75;
        make_tile 39 "property" "Boardwalk" 400;
      ];
  ]

(*let board_tests = []*)

let suite =
  "Monopoly Test Suite: "
  >::: List.flatten [ locations_tests (*; board_tests*) ]

let _ = run_test_tt_main suite
