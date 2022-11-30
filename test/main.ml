open OUnit2
open Monopoly
open Locations
open Board
open Player
open Cards

let data_dir_prefix = "data" ^ Filename.dir_sep
let mono = Yojson.Basic.from_file (data_dir_prefix ^ "Monopoly.json")
(*let cards = Yojson.Basic.from_file (data_dir_prefix ^ "Cards.json")*)

(****************************************************************************
  Helper Functions
  ***************************************************************************)

let tiles_list_test name json expected : test =
  name >:: fun _ -> assert_equal expected (tiles_list json)

let find_tile_test name index tiles expected : test =
  name >:: fun _ -> assert_equal expected (find_tile index tiles)

let check_single_roll result = result >= 1 && result <= 6

let roll_dice_test name expected : test =
  name >:: fun _ ->
  assert_equal expected
    (check_single_roll (roll_dice ()))
    ~printer:string_of_bool

let init_player_test name nm expected : test =
  name >:: fun _ -> assert_equal expected (init_player nm)

let card_display_info_test name ct nm flvr_txt acts expected : test =
  name >:: fun _ ->
  assert_equal expected (init_card ct nm flvr_txt acts |> card_display_info)

(****************************************************************************
  End of Helper Functions
  ***************************************************************************)
let locations_tests =
  [
    tiles_list_test "tiles_list_test: full monopoly board" mono
      [
        make_tile 0 "go" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 1 "property" "Mediterranean Avenue" (tile_color "brown") 60 50
          2 10 30 90 160 250 0;
        make_tile 2 "cc" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 3 "property" "Baltic Avenue" (tile_color "brown") 60 50 4 20
          60 180 320 450 0;
        make_tile 4 "tax" "Income Tax" (tile_color "colorless") 0 0 0 0 0 0 0 0
          200;
        make_tile 5 "railroad" "Reading Railroad" (tile_color "colorless") 200 0
          25 50 100 200 0 0 0;
        make_tile 6 "property" "Oriental Avenue" (tile_color "light blue") 100
          50 6 30 90 270 400 550 0;
        make_tile 7 "chance" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 8 "property" "Vermont Avenue" (tile_color "light blue") 100 50
          6 30 90 270 400 550 0;
        make_tile 9 "property" "Connecticut Avenue" (tile_color "light blue")
          120 50 8 40 100 300 450 600 0;
        make_tile 10 "visiting jail" "" (tile_color "colorless") 0 0 0 0 0 0 0 0
          0;
        make_tile 11 "property" "St. Charles Place" (tile_color "pink") 140 100
          10 50 150 450 625 750 0;
        make_tile 12 "utility" "Electric Company" (tile_color "colorless") 150 0
          0 0 0 0 0 0 0;
        make_tile 13 "property" "States Avenue" (tile_color "pink") 140 100 10
          50 150 450 625 750 0;
        make_tile 14 "property" "Virginia Avenue" (tile_color "pink") 160 100 12
          60 180 500 700 900 0;
        make_tile 15 "railroad" "Pennsylvania Railroad" (tile_color "colorless")
          200 0 25 50 100 200 0 0 0;
        make_tile 16 "property" "St. James Place" (tile_color "orange") 180 100
          14 70 200 550 750 950 0;
        make_tile 17 "cc" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 18 "property" "Tennessee Avenue" (tile_color "orange") 180 100
          14 70 200 550 750 950 0;
        make_tile 19 "property" "New York Avenue" (tile_color "orange") 200 100
          16 80 220 600 800 1000 0;
        make_tile 20 "parking" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 21 "property" "Kentucky Avenue" (tile_color "red") 220 150 18
          90 250 700 875 1050 0;
        make_tile 22 "chance" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 23 "property" "Indiana Avenue" (tile_color "red") 220 150 18
          90 250 700 875 1050 0;
        make_tile 24 "property" "Illinois Avenue" (tile_color "red") 240 150 20
          100 300 750 925 1100 0;
        make_tile 25 "railroad" "B & O Railroad" (tile_color "colorless") 200 0
          25 50 100 200 0 0 0;
        make_tile 26 "property" "Atlantic Avenue" (tile_color "yellow") 260 150
          22 110 330 800 975 1150 0;
        make_tile 27 "property" "Ventnor Avenue" (tile_color "yellow") 260 150
          22 110 330 800 975 1150 0;
        make_tile 28 "utility" "Water Works" (tile_color "colorless") 150 0 0 0
          0 0 0 0 0;
        make_tile 29 "property" "Marvin Gardens" (tile_color "yellow") 280 150
          24 120 360 850 1025 1200 0;
        make_tile 30 "jail" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 31 "property" "Pacific Avenue" (tile_color "green") 300 200 26
          130 390 900 1100 1275 0;
        make_tile 32 "property" "North Carolina Avenue" (tile_color "green") 300
          200 26 130 390 900 1100 1275 0;
        make_tile 33 "cc" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 34 "property" "Pennsylvania Avenue" (tile_color "green") 320
          200 28 150 450 1000 1200 1400 0;
        make_tile 35 "railroad" "Short Line" (tile_color "colorless") 200 0 25
          50 100 200 0 0 0;
        make_tile 36 "chance" "" (tile_color "colorless") 0 0 0 0 0 0 0 0 0;
        make_tile 37 "property" "Park Place" (tile_color "dark blue") 350 200 35
          175 500 1100 1300 1500 0;
        make_tile 38 "tax" "Luxury Tax" (tile_color "colorless") 0 0 0 0 0 0 0 0
          75;
        make_tile 39 "property" "Boardwalk" (tile_color "dark blue") 400 200 50
          200 600 1400 1700 2000 0;
      ];
    find_tile_test "find_tile_test: go" 0 (tiles_list mono) Go;
    find_tile_test "find_tile_test: property" 1 (tiles_list mono)
      (make_contents "Mediterranean Avenue" (tile_color "brown") 60 50 2 10 30
         90 160 250);
    find_tile_test "find_tile_test: cc" 2 (tiles_list mono) CommunityChest;
    find_tile_test "find_tile_test: chance" 7 (tiles_list mono) Chance;
    find_tile_test "find_tile_test: jail" 30 (tiles_list mono) Jail;
  ]

let board_tests =
  [
    roll_dice_test "roll_dice_test: 1st roll" true;
    roll_dice_test "roll_dice_test: 2nd roll" true;
    roll_dice_test "roll_dice_test: 3rd roll" true;
    roll_dice_test "roll_dice_test: 4th roll" true;
    roll_dice_test "roll_dice_test: 5th roll" true;
    roll_dice_test "roll_dice_test: 6th roll" true;
    roll_dice_test "roll_dice_test: 7th roll" true;
  ]

let player_tests =
  [
    init_player_test "init_player_test: Initial balance and given name" "Joe"
      (make_player 0 "Joe" (make_balance 1500 2 2 2 6 5 5 5) 0);
  ]

let cards_tests =
  [
    card_display_info_test "card_display_info_test : generic card" "chance"
      "Generic Card" "generic flavor text"
      { move = "NA"; pay = "NA"; receive = "NA" }
      "Picked up card Generic Card: generic flavor text";
  ]

let suite =
  "Monopoly Test Suite: "
  >::: List.flatten [ locations_tests; board_tests; player_tests; cards_tests ]

let _ = run_test_tt_main suite
