(** [play_game f] starts the adventure in file [f]. *)
let play_monopoly f = raise (Failure "Unimplemented: Main.play_game")

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nLet's play Monopoly.\n";
  play_monopoly 0

(* Execute the game engine. *)

(* example of main function from a2 : let main () = ANSITerminal.print_string [
   ANSITerminal.red ] "\n\nLet's play Monopoly.\n"; print_endline "Please enter
   the name of the game file you want to load.\n"; print_string "> "; match
   read_line () with | exception End_of_file -> () | file_name -> play_monopoly
   (data_dir_prefix ^ file_name ^ ".json")*)

let () = main ()
