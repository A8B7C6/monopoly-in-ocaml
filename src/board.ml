open Player
open Locations

let roll_dice () =
  let _ = Random.init (int_of_float (Unix.gettimeofday ())) in
  Random.int 6 + 1

let monopoly = Yojson.Basic.from_file "data/Monopoly.json"
let monopoly_list = tiles_list monopoly

(* TODO: create the list for Cards.json. Should call a function from Cards*)

let do_turn player =
  print_endline (player.name ^ ", your turn has begun");
  let frst = roll_dice () in
  let scnd = roll_dice () in
  let total = frst + scnd in
  print_endline
    ("You rolled a " ^ string_of_int frst ^ " and a " ^ string_of_int scnd);
  print_endline
    ("Your new board position is " ^ get_tile_name total monopoly_list);
  { player with board_position = (player.board_position + total) mod 40 }