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

let multiply_str str num =
  let rec helper str num final =
    if num = 0 then final else helper str (num - 1) (final ^ str)
  in
  helper str num ""

let adjust_str str target_length =
  if target_length - String.length str > 0 then
    str ^ multiply_str " " (target_length - String.length str)
  else String.sub str 0 target_length

let print_border () = print_endline (multiply_str "+---------------+" 11)

let print_row fst last top_row =
  let rec helper fst last final =
    let str =
      if top_row then " Tile: " ^ string_of_int fst
      else
        let tile_no = last - 10 + (last - fst) in
        if tile_no = 40 then " Tile: " ^ string_of_int 0
        else " Tile: " ^ string_of_int tile_no
    in
    if fst > last then final
    else final ^ adjust_str str 15 ^ "|" ^ helper (fst + 1) last final
  in
  print_endline (helper fst last "|")

let print_side_rows () =
  let rec helper pos rpos =
    let str_left = "| Tile: " ^ string_of_int pos in
    let str_right = "| Tile: " ^ string_of_int rpos in
    print_endline
      (adjust_str str_left 15 ^ " ||" ^ multiply_str " " 151 ^ "|"
     ^ adjust_str str_right 15 ^ " ||");
    print_endline
      (adjust_str "| " 15 ^ " ||" ^ multiply_str " " 151 ^ "|"
     ^ adjust_str "| " 15 ^ " ||");
    print_endline
      (adjust_str "|_________________" 16
      ^ "||" ^ multiply_str " " 151 ^ "|"
      ^ adjust_str "|_________________" 16
      ^ "||");
    if pos = 1 then () else helper (pos - 1) (rpos + 1)
  in
  helper 9 21

let print_board () =
  print_border ();
  print_row 10 20 true;
  print_side_rows ();
  print_row 30 40 false;
  print_border ()
(*let info_position player = let tilenm = find_tile (player.board_position)
  monopoly in *)

(* let do_turn (player : Player.player) : Player.player = (* before this we
   should output the player's turn has begun *) let dice1 = roll_dice () in let
   dice2 = roll_dice () in let total_movement = dice1 + dice2 in (* before this
   we should output the player's invidiual dice rolls & then increment their
   position *) let board_position = (get_board_position player) + total_movement
   in (* here we should communicate the new tile the player is on & handle any
   effects of landing on that new tile *) *)
