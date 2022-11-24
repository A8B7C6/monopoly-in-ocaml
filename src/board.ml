open Player
open Locations

(* Helper method utilized to simulate a single dice roll. Returns a number
   utilizing built-in Random module, reseeds the generator with the current UNIX
   time.*)
let roll_dice () =
  let _ = Random.init (int_of_float (Unix.gettimeofday ())) in
  Random.int 6 + 1

let get_tile_name index =
  match find_tile index monopoly_list with
  | Property p -> property_name p
  | Railroad r -> property_name r
  | Utility u -> property_name u
  | Tax t -> tax_name t
  | Go -> "go"
  | CommunityChest -> "Community Chest"
  | Chance -> "Chance"
  | Jail -> "Jail"
  | VisitingJail -> "Visiting Jail"
  | Parking -> "Parking"

let do_turn player =
  print_endline (player.name ^ ", your turn has begun");
  let frst = roll_dice () in
  let scnd = roll_dice () in
  let total = frst + scnd in
  print_endline
    ("You rolled a " ^ string_of_int frst ^ " and a " ^ string_of_int scnd);
  print_endline ("Your new board position is " ^ get_tile_name total);
  { player with board_position = (player.board_position + total) mod 40 }