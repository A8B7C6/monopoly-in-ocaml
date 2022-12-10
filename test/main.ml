open OUnit2
open Monopoly
open Locations
open Board
open Player
open Cards

let data_dir_prefix = "data" ^ Filename.dir_sep
let mono = Yojson.Basic.from_file (data_dir_prefix ^ "Monopoly.json")
let cards = Yojson.Basic.from_file (data_dir_prefix ^ "Cards.json")

(****************************************************************************
  Helper Functions
  ***************************************************************************)

let tiles_list_test name json expected : test =
  name >:: fun _ -> assert_equal expected (tiles_list json)

let find_tile_test name index tiles expected : test =
  name >:: fun _ -> assert_equal expected (find_tile index tiles)

let get_tile_name_test name index mlist expected_name : test =
  name >:: fun _ -> assert_equal expected_name (get_tile_name index mlist)

let check_single_roll result = result >= 1 && result <= 6

let roll_dice_test name expected : test =
  name >:: fun _ ->
  assert_equal expected
    (check_single_roll (roll_dice ()))
    ~printer:string_of_bool

let init_player_test name nm expected : test =
  name >:: fun _ -> assert_equal expected (init_player nm)

let card_display_info_test name ct nm flvr_txt mv rcv gtj ooj expected : test =
  name >:: fun _ ->
  assert_equal expected
    (init_card ct nm flvr_txt mv rcv gtj ooj |> card_display_info)

let make_chance_list_test name cards expected : test =
  name >:: fun _ -> assert_equal expected (make_chance_list cards)

let make_cc_list_test name cards expected : test =
  name >:: fun _ -> assert_equal expected (make_cc_list cards)

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
    get_tile_name_test "get_tile_name_test w/ first tile" 0 (tiles_list mono)
      "Go";
    get_tile_name_test "get_tile_name_test w/ tax tile" 4 (tiles_list mono)
      "Income Tax";
    get_tile_name_test "get_tile_name_test w/ chance tile" 7 (tiles_list mono)
      "Chance";
    get_tile_name_test "get_tile_name_test w/ parking tile" 20 (tiles_list mono)
      "Parking";
    get_tile_name_test "get_tile_name_test w/ utility tile" 28 (tiles_list mono)
      "Water Works";
    get_tile_name_test "get_tile_name_test w/ jail tile" 30 (tiles_list mono)
      "Jail";
    get_tile_name_test "get_tile_name_test w/ visiting jail tile" 10
      (tiles_list mono) "Visiting Jail";
    get_tile_name_test "get_tile_name_test w/ last tile" 39 (tiles_list mono)
      "Boardwalk";
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
    ( "trying something" >:: fun _ ->
      assert_equal 9
        (let player = init_player "jo" in
         Board.do_turn 3 6 player;
         player.board_position) );
  ]

let player_tests =
  [
    init_player_test "init_player_test: Initial balance and given name" "Joe"
      (make_player 0 "Joe" (make_balance 1500 2 2 2 6 5 5 5) 0);
    (* TODO: do we want to prohibit empty names? *)
    init_player_test "init_player_test: Initial balance and empty name" ""
      (make_player 0 "" (make_balance 1500 2 2 2 6 5 5 5) 0);
  ]

let cards_tests =
  [
    card_display_info_test "card_display_info_test : generic card" Chance
      "Generic Card" "generic flavor text" "NA" 0 false false
      "Picked up card Generic Card: generic flavor text";
    make_chance_list_test "make_chance_list_test : full chance cards list"
      (parse cards)
      [
        init_card Chance "Move" "Advance to Go. (Collect $200)" "Go" 0 false
          false;
        init_card Chance "Move"
          "Advance to Illinois Avenue. If you pass Go, collect $200."
          "Illinois Avenue" 0 false false;
        init_card Chance "Move"
          "Advance to St. Charles Place. If you pass Go, collect $200."
          "St. Charles Place" 0 false false;
        init_card Chance "Move"
          "Advance token to the nearest Utility. If unowned, you may buy it \
           from the Bank. If owned, throw dice and pay owner a total 10 (ten) \
           times the amount thrown."
          "Utility" 0 false false;
        init_card Chance "Move"
          "Advance to the nearest Railroad. If unowned, you may buy it from \
           the Bank. If owned, pay owner twice the rental to which they are \
           otherwise entitled."
          "Railroad" 0 false false;
        init_card Chance "Balance Change" "Bank pays you dividend of $50." "NA"
          50 false false;
        init_card Chance "Get Out of Jail Free Card"
          "Get out of Jail Free. This card may be kept until needed, or \
           traded/sold."
          "NA" 0 false true;
        init_card Chance "Move" "Go Back Three Spaces." "Back 3" 0 false false;
        init_card Chance "Go to Jail"
          "Go to Jail. Go directly to Jail. Do not pass Go, do not collect \
           $200."
          "NA" 0 true false;
        init_card Chance "Balance Change"
          "You forgot to mow your lawn, drawing your HOA's ire. Pay a fine of \
           $75."
          "NA" (-75) false false;
        init_card Chance "Move"
          "Take a trip to Reading Railroad. If you pass Go, collect $200."
          "Reading Railroad" 0 false false;
        init_card Chance "Move"
          "Take a walk on the Boardwalk. Advance token to Boardwalk."
          "Boardwalk" 0 false false;
        init_card Chance "Balance Change"
          "You have been elected Chairman of the Board. Pay $100." "NA" (-100)
          false false;
        init_card Chance "Balance Change"
          "Your building loan matures. Receive $150." "NA" 150 false false;
      ];
    make_cc_list_test "make_cc_list_test : full cc cards list" (parse cards)
      [
        init_card CC "Move" "Advance to Go. (Collect $200)" "Go" 0 false false;
        init_card CC "Balance Change" "Bank error in your favor. Collect $200."
          "NA" 200 false false;
        init_card CC "Balance Change" "Doctor's fees. Pay $50." "NA" (-50) false
          false;
        init_card CC "Balance Change" "From the sale of a stock you get $50."
          "NA" 50 false false;
        init_card CC "Get Out of Jail Free Card"
          "Get out of Jail Free. This card may be kept until needed, or \
           traded/sold."
          "NA" 0 false true;
        init_card CC "Go to Jail"
          "Go to Jail. Go directly to Jail. Do not pass Go, do not collect \
           $200."
          "NA" 0 true false;
        init_card CC "Balance Change" "Grand Opera Night. Collect $125." "NA"
          125 false false;
        init_card CC "Balance Change" "Holiday Fund matures. Receive $100." "NA"
          100 false false;
        init_card CC "Balance Change" "Income tax refund. Collect $20." "NA" 20
          false false;
        init_card CC "Balance Change"
          "It's your birthday. Collect $25 as a gift." "NA" 25 false false;
        init_card CC "Balance Change" "Life insurance matures. Collect $100."
          "NA" 100 false false;
        init_card CC "Balance Change" "Hospital fees. Pay $50." "NA" (-50) false
          false;
        init_card CC "Balance Change" "School fees. Pay $50." "NA" (-50) false
          false;
        init_card CC "Balance Change" "Receive $25 consultancy fee." "NA" 25
          false false;
        init_card CC "Balance Change"
          "You are assessed for street repairs: Pay $155." "NA" (-155) false
          false;
        init_card CC "Balance Change"
          "You have won second prize in a beauty contest. Collect $10." "NA" 10
          false false;
        init_card CC "Balance Change" "You inherit $100." "NA" 100 false false;
      ];
  ]

let suite =
  "Monopoly Test Suite: "
  >::: List.flatten [ locations_tests; board_tests; player_tests; cards_tests ]

let _ = run_test_tt_main suite
