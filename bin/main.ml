open Monopoly
open Locations
open Player
open Board
(** [play_game f] starts the adventure in file [f]. *)
let data_dir_prefix = "data" ^ Filename.dir_sep

let do_turn (player : _player) =
  let cur = player.board_position in
  let new_pos = cur + roll_dice () in
  if new_pos > 39 then player.board_position <- new_pos - 39
  else player.board_position <- new_pos

  let locations_list j = tiles_list j
let player_one = print_endline "What is your name?"; let name = read_line () in init_player name


let play_monopoly = let mono = to_json "data/Monopoly.json" in let ll = tiles_list mono in 
 let p1 = player_one in (do_turn p1) ; print_endline (p1.board_position |> string_of_int)
(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nLet's play Monopoly.\n";
  play_monopoly

(* Execute the game engine. *)

(* example of main function from a2 : let main () = ANSITerminal.print_string [
   ANSITerminal.red ] "\n\nLet's play Monopoly.\n"; print_endline "Please enter
   the name of the game file you want to load.\n"; print_string "> "; match
   read_line () with | exception End_of_file -> () | file_name -> play_monopoly
   (data_dir_prefix ^ file_name ^ ".json")*)

let () = main ()
