open Player
open Locations
open Cards

let roll_dice () =
  let _ = Random.init (int_of_float (Unix.gettimeofday ())) in
  Random.int 6 + 1

let data_dir_prefix = "data" ^ Filename.dir_sep
let monopoly = Yojson.Basic.from_file (data_dir_prefix ^ "Monopoly.json")
let monopoly_list = tiles_list monopoly
let cards = Yojson.Basic.from_file (data_dir_prefix ^ "Cards.json")
let list_of_cards_json = card_list cards

let do_turn (player : _player) =
  print_endline (player.name ^ ", your turn has begun");
  let frst = roll_dice () in
  let scnd = roll_dice () in
  let total = frst + scnd in
  print_endline
    ("You rolled a " ^ string_of_int frst ^ " and a " ^ string_of_int scnd);
  print_endline
    ("Your new board position is " ^ get_tile_name total monopoly_list);
  { player with board_position = (player.board_position + total) mod 40 }