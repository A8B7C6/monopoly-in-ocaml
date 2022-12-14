open OUnit2
open Monopoly
open Locations
open Board
open Player
open Cards
open Bank

let data_dir_prefix = "data" ^ Filename.dir_sep
let mono = Yojson.Basic.from_file (data_dir_prefix ^ "Monopoly.json")
let cards = Yojson.Basic.from_file (data_dir_prefix ^ "Cards.json")

(******************************** Testing Plan ********************************
  OUnit Tests were made the modules: Bank, Board, Cards, Locations, Player, Rent

  Black box testing and glass box testing were applied. Bisect was also
  implemented, and tests were written to cover all functions we could write
  tests for in src/ that were not tested manually. As the large number of state
  transitions across modules makes it impracticable to prove correctness by
  testing every single permutation, our group's test suite ensures the
  correctness of each function to prove the correctness of the program.

  Manual Tests were conducted the modules: GUIhelper, bin/Main

  Manual testing through playing the game was extensively applied to the two
  modules in order to check for their correctness.

  *******************************************************************************)

(****************************************************************************
  Helper Functions
  ***************************************************************************)

let tiles_list_test name json expected : test =
  name >:: fun _ -> assert_equal expected (tiles_list json)

let find_tile_test name index tiles expected : test =
  name >:: fun _ ->
  assert_equal expected (find_tile index tiles) ~printer:(fun x ->
      match x with
      | Property _ -> "property"
      | Railroad _ -> "railroad"
      | Utility _ -> "utility"
      | Tax _ -> "tax"
      | Go -> "go"
      | CommunityChest -> "community chest"
      | Chance -> "chance"
      | Jail -> "jail"
      | VisitingJail -> "visiting jail"
      | Parking -> "parking")

let get_tile_name_test name index mlist expected_name : test =
  name >:: fun _ ->
  assert_equal expected_name (get_tile_name index mlist) ~printer:(fun x -> x)

let check_single_roll result = result >= 1 && result <= 6

let roll_dice_test name expected : test =
  name >:: fun _ ->
  assert_equal expected
    (check_single_roll (roll_dice ()))
    ~printer:string_of_bool

let check_for_double_test name player dice1 dice2 expected : test =
  let _ = check_for_double player dice1 dice2 in
  name >:: fun _ ->
  assert_equal expected player.doubles ~printer:(fun x -> string_of_int x)

let init_player_test name nm expected : test =
  name >:: fun _ -> assert_equal expected (init_player nm)

let get_name_test name brdpos nm blnce dbls expected : test =
  name >:: fun _ ->
  assert_equal expected (get_name (make_player brdpos nm blnce dbls))

let get_board_position_test name brdpos nm blnce dbls expected : test =
  name >:: fun _ ->
  assert_equal expected (get_board_position (make_player brdpos nm blnce dbls))

let shuffle_player_test name (nms : string list) expected : test =
  let plst = List.map init_player nms in
  name >:: fun _ -> assert_equal expected (shuffle_player plst)

let card_display_info_test name ct nm flvr_txt mv rcv gtj ooj expected : test =
  name >:: fun _ ->
  assert_equal expected
    (init_card ct nm flvr_txt mv rcv gtj ooj |> card_display_info)

let make_chance_list_test name crdlst expected : test =
  name >:: fun _ -> assert_equal expected (make_chance_list crdlst)

let make_cc_list_test name crdlst expected : test =
  name >:: fun _ -> assert_equal expected (make_cc_list crdlst)

let find_min_test name (expected_min : 'a * 'b) lst : test =
  name >:: fun _ -> assert_equal expected_min (find_min (ref (List.hd lst)) lst)

let chance_mv_test name brdpos playernm dbls ct nm flvr_txt mv rcv gtj ooj
    expected : test =
  let pl1 =
    make_player brdpos playernm (make_balance 1500 2 2 2 6 5 5 5) dbls
  in
  name >:: fun _ ->
  assert_equal expected
    (chance_mv (init_card ct nm flvr_txt mv rcv gtj ooj) pl1;
     get_board_position pl1)
    ~printer:string_of_int

let to_bottom_test name ct nm flvr_txt mv rcv gtj ooj (crdlst : card list ref)
    expected : test =
  let rec find_last lst =
    match lst with
    | [] -> failwith "No cards"
    | [ c ] -> c
    | _ :: t -> find_last t
  in
  name >:: fun _ ->
  assert_equal expected
    (find_last
       (to_bottom (init_card ct nm flvr_txt mv rcv gtj ooj) crdlst;
        !crdlst))

let remove_jail_test name ct (crdlst : card list ref) : test =
  let ooj_crd =
    init_card ct "Get Out of Jail Free Card"
      "Get out of Jail Free. This card may be kept until needed, or \
       traded/sold."
      "NA" 0 false false
  in
  let check_ooj_crd_presence lst =
    if List.filter (fun x -> x = ooj_crd) lst = lst then true else false
  in
  name >:: fun _ ->
  assert_equal false
    (check_ooj_crd_presence
       (remove_jail ooj_crd crdlst;
        !crdlst))

let rec print_list l f =
  match l with
  | [] -> "\n"
  | h :: t -> f h ^ "  " ^ print_list t f

let bank_test name input expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output input ~printer:(fun b ->
      let bl =
        [ b.total; b.fivehun; b.hun; b.ffty; b.twnty; b.tens; b.fives; b.ones ]
      in
      let string_balance sb = print_list sb string_of_int in
      string_balance bl)

let deduct_bank_test name u i expected_output =
  bank_test name (deduct_from_balance (init_balance u) i) expected_output

let add_bank_test name u i expected_output =
  bank_test name (add_to_balance (init_balance u) i) expected_output

let init_player_test_fails name player : test =
  name >:: fun _ ->
  assert_raises (Failure "empty string") (fun () -> init_player player)

let remove_player_test name player player_list expected_list =
  name >:: fun _ ->
  let pl_list = remove_player player player_list in
  assert_equal (List.length expected_list) (List.length pl_list)

let update_last_dice_roll_test name player expected : test =
  name >:: fun _ ->
  let _ = update_last_dice_roll player expected in
  assert_equal player.last_dice_roll expected

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
      (make_contents "property" "Mediterranean Avenue" (tile_color "brown") 60
         50 2 10 30 90 160 250 0);
    find_tile_test "find_tile_test: tax" 38 (tiles_list mono)
      (make_contents "tax" "Luxury Tax" (tile_color "colorless") 0 0 0 0 0 0 0 0
         75);
    find_tile_test "find_tile_test: utility" 12 (tiles_list mono)
      (make_contents "utility" "Electric Company" (tile_color "colorless") 150 0
         0 0 0 0 0 0 0);
    find_tile_test "find_tile_test: railroad" 15 (tiles_list mono)
      (make_contents "railroad" "Pennsylvania Railroad" (tile_color "colorless")
         200 0 25 50 100 200 0 0 0);
    find_tile_test "find_tile_test: cc" 2 (tiles_list mono) CommunityChest;
    find_tile_test "find_tile_test: chance" 7 (tiles_list mono) Chance;
    find_tile_test "find_tile_test: jail" 30 (tiles_list mono) Jail;
    find_tile_test "find_tile_test: jail" 10 (tiles_list mono) VisitingJail;
    find_tile_test "find_tile_test: jail" 20 (tiles_list mono) Parking;
    get_tile_name_test "get_tile_name_test w/ first tile" 0 (tiles_list mono)
      "Go";
    get_tile_name_test "get_tile_name_test w/ tax tile" 4 (tiles_list mono)
      "Income Tax";
    get_tile_name_test "get_tile_name_test w/ chance tile" 7 (tiles_list mono)
      "Chance";
    get_tile_name_test "get_tile_name_test w/ cc tile" 33 (tiles_list mono)
      "Community Chest";
    get_tile_name_test "get_tile_name_test w/ parking tile" 20 (tiles_list mono)
      "Parking";
    get_tile_name_test "get_tile_name_test w/ utility tile" 28 (tiles_list mono)
      "Water Works";
    get_tile_name_test "get_tile_name_test w/ railroad tile" 5 (tiles_list mono)
      "Reading Railroad";
    get_tile_name_test "get_tile_name_test w/ jail tile" 30 (tiles_list mono)
      "Jail";
    get_tile_name_test "get_tile_name_test w/ visiting jail tile" 10
      (tiles_list mono) "Visiting Jail";
    get_tile_name_test "get_tile_name_test w/ last tile" 39 (tiles_list mono)
      "Boardwalk";
  ]

let bank_tests =
  [
    bank_test "init_balance has appropriate values" (init_balance ())
      (make_balance 1500 2 2 2 6 5 5 5);
    add_bank_test "add $8 to init_balance" () 8
      (make_balance 1508 2 2 2 6 5 6 8);
    add_bank_test "add $18 to init_balance" () 18
      (make_balance 1518 2 2 2 6 6 6 8);
    add_bank_test "add $28 to init_balance" () 28
      (make_balance 1528 2 2 2 7 5 6 8);
    add_bank_test "add $58 to init_balance" () 58
      (make_balance 1558 2 2 3 6 5 6 8);
    add_bank_test "add $88 to init_balance" () 88
      (make_balance 1588 2 2 3 7 6 6 8);
    add_bank_test "add $108 to init_balance" () 108
      (make_balance 1608 2 3 2 6 5 6 8);
    add_bank_test "add $808 to init_balance" () 808
      (make_balance 2308 3 5 2 6 5 6 8);
    add_bank_test "add $888 to init_balance" () 888
      (make_balance 2388 3 5 3 7 6 6 8);
    deduct_bank_test "remove $8 from init_balance" () 8
      (make_balance 1492 2 2 2 6 5 4 2);
    deduct_bank_test "remove $96 from init_balance" () 96
      (make_balance 1404 2 2 2 5 0 0 4);
    deduct_bank_test "remove $888 from init_balance" () 888
      (make_balance 612 1 1 0 0 1 0 2);
    deduct_bank_test "remove -$1 from (aka add $1 to) init_balance" () (-1)
      (make_balance 1501 2 2 2 6 5 5 6);
    deduct_bank_test "remove $0 from init_balance" () 0
      (make_balance 1500 2 2 2 6 5 5 5);
  ]

let intList = [ (0, 0); (2, 2); (3, 3); (4, 4); (5, 5) ]
let intList_One_Elem = [ (1, 1) ]
let stringList = [ ("b", "b"); ("a", "a") ]
let stringList_One_Elem = [ ("b", "b") ]
let reverse_intList = [ (5, 5); (4, 4); (3, 3); (2, 2); (0, 0) ]

let find_min_tests =
  [
    find_min_test "Find min test with multiple int list" (0, 0) intList;
    find_min_test "Find min test with one element int list" (1, 1)
      intList_One_Elem;
    find_min_test "Find min test with multiple string list" ("a", "a")
      stringList;
    find_min_test "Find min test with one element string list" ("b", "b")
      stringList_One_Elem;
    find_min_test "Find min test with reverse order  int list" (0, 0)
      reverse_intList;
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
    ( "do_turn" >:: fun _ ->
      assert_equal 9
        (let player = init_player "jo" in
         Board.do_turn 3 6 player;
         player.board_position) );
    ( "looping do_turn" >:: fun _ ->
      assert_equal 1
        (let player = init_player "jo" in
         Board.do_turn 6 5 player;
         Board.do_turn 6 4 player;
         Board.do_turn 4 6 player;
         Board.do_turn 5 5 player;
         player.board_position) );
    chance_mv_test "chance_mv_test: Boardwalk (39)" 7 "Walker" 0 Chance "Move"
      "Take a walk on the Boardwalk. Advance token to Boardwalk." "Boardwalk" 0
      false false 39;
    chance_mv_test "chance_mv_test: Go (0)" 7 "Gofer" 0 Chance "Move"
      "Advance to Go. (Collect $200)" "Go" 0 false false 0;
    chance_mv_test "chance_mv_test: Illinois Avenue (24)" 7 "Illyanna" 0 Chance
      "Move" "Advance to Illinois Avenue. If you pass Go, collect $200."
      "Illinois Avenue" 0 false false 24;
    chance_mv_test "chance_mv_test: St. Charles Place (11)" 7 "Charlie" 0 Chance
      "Move" "Advance to St. Charles Place. If you pass Go, collect $200."
      "St. Charles Place" 0 false false 11;
    chance_mv_test "chance_mv_test: Reading Railroad (5)" 22 "Reader" 0 Chance
      "Move" "Take a trip to Reading Railroad. If you pass Go, collect $200."
      "Reading Railroad" 0 false false 5;
    chance_mv_test "chance_mv_test: Nearest Railroad (5)" 7 "Disraeli 1" 0
      Chance "Move"
      "Advance to the nearest Railroad. If unowned, you may buy it from the \
       Bank. If owned, pay owner twice the rental to which they are otherwise \
       entitled."
      "Railroad" 0 false false 5;
    chance_mv_test "chance_mv_test: Nearest Railroad (25)" 22 "Disraeli 2" 0
      Chance "Move"
      "Advance to the nearest Railroad. If unowned, you may buy it from the \
       Bank. If owned, pay owner twice the rental to which they are otherwise \
       entitled."
      "Railroad" 0 false false 25;
    chance_mv_test "chance_mv_test: Nearest Railroad (35)" 36 "Disraeli 3" 0
      Chance "Move"
      "Advance to the nearest Railroad. If unowned, you may buy it from the \
       Bank. If owned, pay owner twice the rental to which they are otherwise \
       entitled."
      "Railroad" 0 false false 35;
    chance_mv_test "chance_mv_test: Nearest Utility (12)" 7 "Uris 1" 0 Chance
      "Move"
      "Advance token to the nearest Utility. If unowned, you may buy it from \
       the Bank. If owned, throw dice and pay owner a total 10 (ten) times the \
       amount thrown."
      "Utility" 0 false false 12;
    chance_mv_test "chance_mv_test: Nearest Utility (28)" 36 "Uris 2" 0 Chance
      "Move"
      "Advance token to the nearest Utility. If unowned, you may buy it from \
       the Bank. If owned, throw dice and pay owner a total 10 (ten) times the \
       amount thrown."
      "Utility" 0 false false 28;
    chance_mv_test "chance_mv_test: Back 3 (36 - 3 = 33)" 36 "Walker" 0 Chance
      "Move" "Go Back Three Spaces." "Back 3" 0 false false 33;
  ]
  @ find_min_tests

let jailed_tests : test list =
  let jailed_player = init_player "jailed" in
  let () = jailed_player.in_jail <- true in
  [
    (let _ =
       check_for_double_test
         "Check for doubles frees jailed player with same dice roll"
         jailed_player 1 1 1
     in
     "jailed player should be freed from jail" >:: fun _ ->
     assert_equal jailed_player.in_jail false);
  ]

let doubles_tests =
  [
    check_for_double_test "Check for doubles increases with same dice roll"
      (init_player "john") 1 1 1;
    check_for_double_test
      "Check for doubles does not increase with different dice roll"
      (init_player "john") 1 1 1;
  ]
  @ jailed_tests

let mockPlayer1 = init_player "John"
let mockPlayer2 = init_player "Nguyen"
let mockPlayer3 = init_player "Savitta"
let mockPlayer4 = init_player "Kitil"
let playerList = [ mockPlayer1; mockPlayer2; mockPlayer3; mockPlayer4 ]
let test1List = [ mockPlayer2; mockPlayer3; mockPlayer4 ]
let test2List = [ mockPlayer3; mockPlayer4 ]
let test3List = [ mockPlayer4 ]
let test4List = []
let mockPlayer5 = init_player "Uh,oh"

let remove_player_tests =
  [
    remove_player_test "Test remove john from player: " mockPlayer1 playerList
      test1List;
    remove_player_test "Test remove Nguyen from player: " mockPlayer2 test1List
      test2List;
    remove_player_test "Test remove Savitta from player: " mockPlayer3 test2List
      test3List;
    remove_player_test "Test remove Kitil from player: " mockPlayer4 test3List
      test4List;
    remove_player_test "Test remove player already removed: " mockPlayer3
      test3List test3List;
    remove_player_test "Test remove player not in game: " mockPlayer5 playerList
      playerList;
  ]

let player_tests =
  [
    init_player_test_fails
      "Init player test fails with empty string for player " "";
    init_player_test "init_player_test: Initial balance and given name" "Joe"
      (make_player 0 "Joe" (init_balance ()) 0);
    get_name_test "get_name_test: A9B7C8" 0 "A9B7C8" (init_balance ()) 0
      "A9B7C8";
    get_name_test "get_name_test: !@#$$%^&*[]{}|:;'/?.,`~" 0
      "!@#$$%^&*[]{}|:;'/?.,`~" (init_balance ()) 0 "!@#$$%^&*[]{}|:;'/?.,`~";
    get_board_position_test "get_board_position_test: 0 (Go)" 0 "Grant"
      (init_balance ()) 0 0;
    get_board_position_test
      "get_board_position_test: 15 (Pennsylvania Railroad)" 15 "Rosecrans"
      (init_balance ()) 0 15;
    shuffle_player_test "shuffle_player_test : 1 player - A" [ "A" ]
      (init_player "A", [ init_player "A" ]);
    shuffle_player_test "shuffle_player_test : 2 players - X, Y" [ "X"; "Y" ]
      (init_player "X", [ init_player "Y"; init_player "X" ]);
    shuffle_player_test "shuffle_player_test : 3 players - X, Y, Z"
      [ "X"; "Y"; "Z" ]
      (init_player "X", [ init_player "Y"; init_player "Z"; init_player "X" ]);
    shuffle_player_test "shuffle_player_test : 4 players - A, B, C, D"
      [ "A"; "B"; "C"; "D" ]
      ( init_player "A",
        [ init_player "B"; init_player "C"; init_player "D"; init_player "A" ]
      );
    update_last_dice_roll_test
      "update last dice roll updates with lowest possible dice roll" mockPlayer1
      2;
    update_last_dice_roll_test
      "update last dice roll updates with highest possible dice roll"
      mockPlayer1 12;
    update_last_dice_roll_test "update last dice roll updates with 6 dice roll"
      mockPlayer1 6;
  ]
  @ doubles_tests @ remove_player_tests

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
    to_bottom_test "to_bottom_test: chance card named 'Testing'" Chance
      "Testing" "This is a card used for testing" "NA" 0 false false chance_lst
      (init_card Chance "Testing" "This is a card used for testing" "NA" 0 false
         false);
    to_bottom_test "to_bottom_test: cc card named 'Examining'" CC "Examining"
      "This is a card used for examination" "NA" 0 false false cc_lst
      (init_card CC "Examining" "This is a card used for examination" "NA" 0
         false false);
    remove_jail_test
      "remove_jail_test: Remove Get Out of Jail Free Card from chance cards \
       list"
      Chance chance_lst;
    remove_jail_test
      "remove_jail_test: Remove Get Out of Jail Free Card from cc cards list" CC
      cc_lst;
  ]

let suite =
  "Monopoly Test Suite: "
  >::: List.flatten
         [ locations_tests; bank_tests; board_tests; player_tests; cards_tests ]

let _ = run_test_tt_main suite
let _ = jailed_tests
