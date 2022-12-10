open Monopoly
open Player
open Board
open Locations
open Guihelper
open Cards

type players = { mutable pl_lst : _player list }

let all_players = { pl_lst = [] }

(**[_cc_card_action] will perform the designated action given in the Community
   Chest card [c] on [player]*)
let _cc_card_action c player =
  let card_name = c.contents.name in
  if card_name = "Get Out of Jail Free Card" then begin
    player.free_jail <- true;
    remove_jail c cc_lst
  end
  else begin
    if card_name = "Move" then
      player.board_position <-
        0 (*also update balance +200 if the player passes Go*)
    else if card_name = "Go to Jail" then begin
      player.in_jail <- true;
      player.board_position <- 30
    end
    else if card_name = "Balance Change" then ();
    to_bottom c cc_lst
  end

(**[_chance_action ] handles actions on the Chance card [_c] on [_player]*)
let _chance_action _c _player = ()

(**[_handle_cc] prints the text on the head of the Community Chest list*)
let _handle_cc player =
  let head = List.hd !chance_lst in
  print_endline (card_display_info head);
  _cc_card_action head player

let handle_card loc player =
  let tile_name = get_tile_name loc monopoly_list in
  if tile_name = "Community Chest" then _handle_cc player
  else if tile_name = "Chance" then _chance_action loc player

let handle_move loc player =
  let card_locs = [ 2; 7; 17; 22; 33; 36 ] in
  if List.mem loc card_locs then handle_card loc player else ()

let do_turn frst scnd player =
  Board.do_turn frst scnd player;
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
