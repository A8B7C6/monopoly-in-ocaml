open Monopoly
open Player
open Board
open Locations
open Guihelper
open Cards
open Rent

type players = { mutable pl_lst : _player list }

let all_players = { pl_lst = [] }

(** [chance_bal] handles updating the balance of [player] according to the
    instructions on the chance card [c]*)
let chance_bal c player =
  let bal = c.contents.actions.balance_change in
  if bal < 0 then deduct_money player (bal * -1) else add_money player bal

(** [chance_jail] moves[player] to jail and updates the player fields (in_jail
    and board_position) accordingly*)
let chance_jail player =
  player.board_position <- 30;
  player.in_jail <- true

(** [_chance_action ] handles the different actions on the Chance card [_c] on
    [_player]*)
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

(** [_cc_action] will perform the designated action given in the community
    chance card [c] on [player]*)
let _cc_action c player =
  let card_name = c.contents.name in
  if card_name = "Get Out of Jail Free Card" then begin
    player.free_jail <- true;
    remove_jail c cc_lst
  end
  else if card_name = "Move" then begin
    player.board_position <- 0;
    add_money player 200;
    to_bottom c cc_lst
  end
  else if card_name = "Go to Jail" then begin
    player.in_jail <- true;
    player.board_position <- 30;
    to_bottom c cc_lst
  end
  else if card_name = "Balance Change" then (
    let bal_change = c.contents.actions.balance_change in
    if bal_change < 0 then deduct_money player (bal_change * -1)
    else add_money player bal_change;
    to_bottom c cc_lst)

(** [_handle_cc] prints the text on the head of the Community Chest list*)
let _handle_cc player =
  let head = List.hd !cc_lst in
  print_endline (card_display_info head);
  _cc_action head player

(** [_handle_chance] prints the text on the head of the Chance card list*)
let _handle_chance player =
  let hd = List.hd !chance_lst in
  print_endline (card_display_info hd);
  _chance_action hd player

(** [handle_card] decides whether the position that the player landed in was for
    Community Chest or Chance*)
let handle_card loc player =
  let tile_name = get_tile_name loc monopoly_list in
  if tile_name = "Community Chest" then _handle_cc player
  else if tile_name = "Chance" then _handle_chance player

(** [handle_move] decides whether the player landed on a location where they
    draw a card or on some other type of tile*)
let handle_move loc player =
  let card_locs = [ 2; 7; 17; 22; 33; 36 ] in
  if List.mem loc card_locs then handle_card loc player
  else check_property player.board_position player

let print_turn_info frst scnd player =
  update_game_data [ player ];
  print_endline
    ("You rolled a " ^ string_of_int frst ^ " and a " ^ string_of_int scnd
   ^ " \n Your new board position is "
    ^ get_tile_name player.board_position monopoly_list
    ^ "\n Your current balance is " ^ "$"
    ^ string_of_int player.balance.total);
  handle_move player.board_position player;
  print_endline
    (" Your updated balance is " ^ "$" ^ string_of_int player.balance.total)

let do_turn frst scnd player =
  Board.do_turn frst scnd player;
  update_last_dice_roll player (frst + scnd);
  let current_doubles = player.doubles in
  check_for_double player frst scnd;
  match player.doubles > current_doubles with
  | false -> print_turn_info frst scnd player
  | _ ->
      print_turn_info frst scnd player;
      print_endline "Because you rolled a double you'll get to go again!";
      do_turn (roll_dice ()) (roll_dice ()) player

let curr_pos_print (player : _player) state =
  let p1name = player.name in
  print_endline
    ("\n" ^ p1name ^ ", your " ^ state ^ " position is "
    ^ (player |> get_board_position |> string_of_int)
    ^ "\n")

let play_monopoly players =
  let rec play_loop continue players =
    let current_player, shuffled_players = shuffle_player players in
    match continue with
    | "y" ->
        curr_pos_print current_player "current";
        (* cc_chance Chance; *)
        let current_player = check_jail_status current_player in
        do_turn (roll_dice ()) (roll_dice ()) current_player;

        curr_pos_print current_player "new";
        update_game_data [ current_player ];
        print_endline "Continue playing? y/n/end game";

        let cont = read_line () in
        if cont = "y" then
          if current_player.balance.total >= 0 then
            play_loop cont shuffled_players
          else
            let removed_list = remove_player current_player shuffled_players in
            if List.length removed_list = 0 then begin
              print_endline "You lose. No more players";
              exit 0
            end
            else play_loop cont (remove_player current_player shuffled_players)
        else play_loop cont players
    | "end game" -> (
        print_endline "do you want to end the game ? yes/no";
        let finish = read_line () in
        match finish with
        | "yes" -> exit 0
        | "no" ->
            print_endline "\n \n Let's continue \n \n";
            play_loop "y" shuffled_players
        | _ -> play_loop "end game" players)
    | _ -> (
        print_endline
          (current_player.name ^ ", do you want to leave the game ? yes/no");
        let leave = read_line () in
        match leave with
        | "yes" ->
            let hold = remove_player current_player shuffled_players in
            print_endline ("\n \n  Good Bye, " ^ current_player.name ^ "\n \n");
            if hold = [] then exit 0 else play_loop "y" hold
        | "no" ->
            print_endline "\n \n Let's continue \n \n";
            play_loop "y" shuffled_players
        | _ -> play_loop "try again" players)
  in
  play_loop "y" players

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  let rec track_player curr_player index =
    print_endline ("Player " ^ curr_player ^ ", what is your name?");
    let name = read_line () in
    match name with
    | "" ->
        print_endline "Name can not be empty";
        track_player curr_player index
    | _ ->
        let pl = init_player name in
        all_players.pl_lst <- all_players.pl_lst @ [ pl ];
        index := !index + 1;
        print_endline "\n"
  in
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nLet's play Monopoly.\n";
  try
    print_endline "How many players are there? (Enter a number)\n";
    let num_players = read_line () |> int_of_string in
    let index = ref 0 in
    while !index < num_players do
      let curr_player = string_of_int (!index + 1) in
      track_player curr_player index
    done;
    play_monopoly all_players.pl_lst
  with Failure _ ->
    print_endline "Please provide a valid number of players.\n";
    main ()

let () = main ()
