open Monopoly
open Player
open Board

let do_turn (player : _player) =
  let cur = player.board_position in
  let dice_roll = roll_dice () in
  print_endline ("You rolled a " ^ (dice_roll |> string_of_int));
  let new_pos = cur + dice_roll in
  if new_pos > 39 then player.board_position <- new_pos - 39
  else player.board_position <- new_pos

let play_monopoly player =
  let p1name = player.name in
  print_endline
    ("\n" ^ p1name ^ ", your current position is "
    ^ (player |> get_board_position |> string_of_int)
    ^ "\n");

  do_turn player;
  print_endline
    (p1name ^ ", your new board position is "
    ^ (player |> get_board_position |> string_of_int))

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nLet's play Monopoly.\n";
  print_endline "What is your name?";
  let name = read_line () in
  let p1 = init_player name in
  play_monopoly p1

(* Execute the game engine. *)

(* example of main function from a2 : let main () = ANSITerminal.print_string [
   ANSITerminal.red ] "\n\nLet's play Monopoly.\n"; print_endline "Please enter
   the name of the game file you want to load.\n"; print_string "> "; match
   read_line () with | exception End_of_file -> () | file_name -> play_monopoly
   (data_dir_prefix ^ file_name ^ ".json")*)

let () = main ()
