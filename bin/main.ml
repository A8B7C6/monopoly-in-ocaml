open Monopoly
open Player
open Board

type players = { mutable pl_lst : _player list }

let all_players = { pl_lst = [] }

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
  print_board ();
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nLet's play Monopoly.\n";

  print_endline "How many players are there? (Enter a number)\n";
  let num_players = read_line () |> int_of_string in
  let index = ref 0 in
  while !index < num_players do
    let curr_player = string_of_int (!index + 1) in
    print_endline ("Player " ^ curr_player ^ ", what is your name?");
    let name = read_line () in
    let pl = init_player name in
    all_players.pl_lst <- all_players.pl_lst @ [ pl ];
    index := !index + 1;
    print_endline "\n"
  done;
  (*need to update play_monopoly to handle a player list*)
  print_endline "What is your name? end of loop need to get rid of this part :/";
  let name = read_line () in
  let p1 = init_player name in
  play_monopoly p1

let () = main ()
