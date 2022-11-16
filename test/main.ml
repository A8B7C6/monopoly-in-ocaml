(* open OUnit2 open Monopoly open Locations open Board *)
(*open Player*)

(* let data_dir_prefix = "data" ^ Filename.dir_sep let mono =
   Yojson.Basic.from_file (data_dir_prefix ^ "Monopoly.json") *)

(****************************************************************************
  Helper Functions
  ***************************************************************************)

(* let tiles_list_test name json expected : test = name >:: fun _ ->
   assert_equal expected (tiles_list json)

   let find_tile_test name index tiles expected : test = name >:: fun _ ->
   assert_equal expected (find_tile index tiles)

   let check_single_roll result = result >= 1 && result <= 6

   let roll_dice_test name expected : test = name >:: fun _ -> assert_equal
   expected (check_single_roll (roll_dice ())) ~printer:string_of_bool *)

(****************************************************************************
  End of Helper Functions
  ***************************************************************************)
(* let locations_tests = [ tiles_list_test "tiles_list_test: full monopoly
   board" mono [ make_tile 0 "go" "" 0; make_tile 1 "property" "Mediterranean
   Avenue" 60; make_tile 2 "cc" "" 0; make_tile 3 "property" "Baltic Avenue" 60;
   make_tile 4 "property" "Income Tax" 200; make_tile 5 "property" "Reading
   Railroad" 200; make_tile 6 "property" "Oriental Avenue" 100; make_tile 7
   "chance" "" 0; make_tile 8 "property" "Vermont Avenue" 100; make_tile 9
   "property" "Connecticut Avenue" 120; make_tile 10 "jail" "" 0; make_tile 11
   "property" "St. Charles Place" 140; make_tile 12 "property" "Electric
   Company" 150; make_tile 13 "property" "States Avenue" 140; make_tile 14
   "property" "Virginia Avenue" 160; make_tile 15 "property" "Pennsylvania
   Railroad" 200; make_tile 16 "property" "St. James Place" 180; make_tile 17
   "cc" "" 0; make_tile 18 "property" "Tennessee Avenue" 180; make_tile 19
   "property" "New York Avenue" 200; make_tile 20 "cc" "" 0; make_tile 21
   "property" "Kentucky Avenue" 220; make_tile 22 "chance" "" 0; make_tile 23
   "property" "Indiana Avenue" 220; make_tile 24 "property" "Illinois Avenue"
   240; make_tile 25 "property" "B & O Railroad" 200; make_tile 26 "property"
   "Atlantic Avenue" 260; make_tile 27 "property" "Ventnor Avenue" 260;
   make_tile 28 "property" "Water Works" 150; make_tile 29 "property" "Marvin
   Gardens" 280; make_tile 30 "jail" "" 0; make_tile 31 "property" "Pacific
   Avenue" 300; make_tile 32 "property" "North Carolina Avenue" 300; make_tile
   33 "cc" "" 0; make_tile 34 "property" "Pennsylvania Avenue" 320; make_tile 35
   "property" "Short Line" 200; make_tile 36 "chance" "" 0; make_tile 37
   "property" "Park Place" 350; make_tile 38 "property" "Luxury Tax" 75;
   make_tile 39 "property" "Boardwalk" 400; ]; find_tile_test "find_tile_test:
   go" 0 (tiles_list mono) Go; find_tile_test "find_tile_test: property" 1
   (tiles_list mono) (make_contents "Mediterranean Avenue" 60); find_tile_test
   "find_tile_test: cc" 2 (tiles_list mono) CommunityChest; find_tile_test
   "find_tile_test: chance" 7 (tiles_list mono) Chance; find_tile_test
   "find_tile_test: jail" 30 (tiles_list mono) Jail; ]

   let board_tests = [ roll_dice_test "roll_dice_test: 1st roll" true;
   roll_dice_test "roll_dice_test: 2nd roll" true; roll_dice_test
   "roll_dice_test: 3rd roll" true; roll_dice_test "roll_dice_test: 4th roll"
   true; roll_dice_test "roll_dice_test: 5th roll" true; roll_dice_test
   "roll_dice_test: 6th roll" true; roll_dice_test "roll_dice_test: 7th roll"
   true; ]

   let suite = "Monopoly Test Suite: " >::: List.flatten [ locations_tests;
   board_tests ]

   let _ = run_test_tt_main suite *)
