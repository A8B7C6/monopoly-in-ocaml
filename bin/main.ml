open Monopoly
open Player
open Board

let do_turn (player : _player) =
  let cur = player.board_position in
  let dice_roll = roll_dice () in
  print_endline ("You rolled a " ^ (dice_roll |> string_of_int));
  let new_pos = cur + dice_roll in
  if new_pos > 39 then player.board_position <- (new_pos mod 39) - 1
  else player.board_position <- new_pos

let curr_pos_print player state =
  let p1name = player.name in
  print_endline
    ("\n" ^ p1name ^ ", your " ^ state ^ " position is "
    ^ (player |> get_board_position |> string_of_int)
    ^ "\n")

let play_monopoly player =
  curr_pos_print player "current";
  let rec play_loop continue =
    match continue with
    | "y" ->
        do_turn player;
        curr_pos_print player "new";
        print_endline "Continue playing? y/n";
        let cont = read_line () in
        play_loop cont
    | _ -> exit 0
  in
  play_loop "y"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nLet's play Monopoly.\n";
  print_endline "What is your name?";
  let name = read_line () in
  let p1 = init_player name in
  play_monopoly p1

let () = main ()
