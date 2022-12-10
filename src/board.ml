open Player
open Locations
open Cards

let roll_dice () =
  let _ = Random.self_init in
  Random.int 6 + 1

let data_dir_prefix = "data" ^ Filename.dir_sep
let monopoly = Yojson.Basic.from_file (data_dir_prefix ^ "Monopoly.json")
let monopoly_list = tiles_list monopoly
let cards = Yojson.Basic.from_file (data_dir_prefix ^ "Cards.json")
let list_of_cards_json = parse cards

let do_turn (frst : int) (scnd : int) (player : _player) =
  let total = frst + scnd in
  if total + player.board_position >= 40 then
    set_board_position player (40 - (total + player.board_position))
  else set_board_position player (total + player.board_position)
