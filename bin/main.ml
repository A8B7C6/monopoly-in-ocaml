open Monopoly
open Player
open Board
open Locations
open Guihelper
open Cards

type players = { mutable pl_lst : _player list }

let all_players = { pl_lst = [] }

let move_new new_index player =
  if player.board_position > new_index then begin
    player.board_position <- new_index;
    distribute_change player 200
  end
  else player.board_position <- new_index

let chance_mv c player =
  let new_loc = c.contents.actions.move in
  if new_loc = "Boardwalk" then player.board_position <- 39
  else if new_loc = "Go" then begin
    player.board_position <- 0;
    distribute_change player 200
  end
  else if new_loc = "Illinois Avenue" then move_new 24 player
  else if new_loc = "St. Charles Place" then move_new 11 player
  else if new_loc = "Reading Railroad" then move_new 5 player
  else if new_loc = "Railroad" || new_loc = "Utility" then ()
  else if new_loc = "Back 3" then ()

let chance_bal c player =
  let bal = c.contents.actions.balance_change in
  if bal < 0 then decrement_balance player (bal * -1)
  else distribute_change player bal

let chance_jail player = player.board_position <- 30

(**[_chance_action ] handles actions on the Chance card [_c] on [_player]*)
let _chance_action c player =
  let name = c.contents.name in
  if name = "Move" then begin
    chance_mv c player;
    to_bottom c chance_lst
  end
  else if name = "Balance Change" then begin
    chance_bal c player;
    to_bottom c chance_lst
  end
  else if name = "Go to Jail" then begin
    chance_jail player;
    to_bottom c chance_lst
  end
  else if name = "Get Out of Jail Free Card" then begin
    player.free_jail <- true;
    remove_jail c chance_lst
  end

(**[_cc_card] will perform the designated action given in the community chance
   card [c] on [player]*)
let _cc_action c player =
  let card_name = c.contents.name in
  if card_name = "Get Out of Jail Free Card" then begin
    player.free_jail <- true;
    remove_jail c cc_lst
  end
  else if card_name = "Move" then begin
    player.board_position <- 0;
    distribute_change player 200;
    to_bottom c cc_lst
  end
  else if card_name = "Go to Jail" then begin
    player.in_jail <- true;
    player.board_position <- 30;
    to_bottom c cc_lst
  end
  else if card_name = "Balance Change" then (
    let bal_change = c.contents.actions.balance_change in
    distribute_change player bal_change;
    to_bottom c cc_lst)

(**[_handle_cc] prints the text on the head of the Community Chest list*)
let _handle_cc player =
  let head = List.hd !cc_lst in
  print_endline (card_display_info head);
  _cc_action head player

let _handle_chance player =
  let hd = List.hd !chance_lst in
  print_endline (card_display_info hd);
  _chance_action hd player

let handle_card loc player =
  let tile_name = get_tile_name loc monopoly_list in
  if tile_name = "Community Chest" then _handle_cc player
  else if tile_name = "Chance" then _handle_chance player

let handle_move loc player =
  let card_locs = [ 2; 7; 17; 22; 33; 36 ] in
  if List.mem loc card_locs then handle_card loc player else ()

let do_turn frst scnd player =
  Board.do_turn frst scnd player;
  let current_doubles = player.doubles in
  check_for_double player frst scnd;
  if player.doubles > current_doubles then print_endline "You rolled a double!";
  print_endline
    ("You rolled a " ^ string_of_int frst ^ " and a " ^ string_of_int scnd);
  print_endline
    ("Your new board position is "
    ^ get_tile_name player.board_position monopoly_list);
  print_endline
    ("Your current balance is is " ^ "$" ^ string_of_int player.balance.total);
  handle_move player.board_position player

let curr_pos_print (player : _player) state =
  let p1name = player.name in
  print_endline
    ("\n" ^ p1name ^ ", your " ^ state ^ " position is "
    ^ (player |> get_board_position |> string_of_int)
    ^ "\n")

let play_monopoly players =
  let rec play_loop continue players =
    match continue with
    | "y" ->
        let current_player, shuffled_players = shuffle_player players in
        curr_pos_print current_player "current";
        (* cc_chance Chance; *)
        let current_player = check_jail_status current_player in
        do_turn (roll_dice ()) (roll_dice ()) current_player;

        curr_pos_print current_player "new";
        update_game_data shuffled_players;
        print_endline "Continue playing? y/n";

        let cont = read_line () in
        play_loop cont shuffled_players
    | _ -> exit 0
  in
  play_loop "y" players

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nLet's play Monopoly.\n";
  try
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
    play_monopoly all_players.pl_lst
  with Failure _ ->
    print_endline "Please provide a valid number of players.\n";
    main ()

let () = main ()
