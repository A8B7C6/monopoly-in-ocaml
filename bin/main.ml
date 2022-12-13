open Monopoly
open Player
open Board
open Locations
open Guihelper
open Cards
open Rent

type players = { mutable pl_lst : _player list }

let all_players = { pl_lst = [] }

(**[move_new] moves [player] to the new board position that is given by
   [new_index]. This function is used for moving the player to new positions
   given by the chance cards and handling their balance if they pass Go*)
let move_new new_index player =
  if player.board_position > new_index then begin
    player.board_position <- new_index;
    add_money player 200
  end
  else player.board_position <- new_index

(**[find_min] finds the minimum in [lst] and stores that tuple in the reference
   [min]. Function is used as a helper in finding the closest Railroad or
   Utility to which a player has to move when drawing a chance card*)
let rec find_min min lst =
  match lst with
  | [] -> !min
  | (a, b) :: t ->
      if b < snd !min then begin
        min := (a, b);
        find_min min t
      end
      else find_min min lst

(**[chance_rail_util loc player] will move [player] to the nearst board position
   that is given by [loc] which will be either 'Railroad' or 'Utilities'.
   Function is used to intitiate the player's move to the nearest board pos
   given by chance card *)
let chance_rail_util loc player =
  let pos = player.board_position in
  let railroads = [ 5; 15; 25; 35 ] in
  let utilities = [ 12; 28 ] in
  if loc = "Railroad" then
    let r2 = List.map (fun x -> (x, Int.abs (x - pos))) railroads in
    let min_ref = ref (List.hd r2) in
    let new_board_pos = find_min min_ref r2 in
    move_new (snd new_board_pos) player
    (*have to check if railroad is owned and if have to pay price*)
  else if loc = "Utilities" then
    let u2 = List.map (fun x -> (x, Int.abs (x - pos))) utilities in
    let min = ref (List.hd u2) in
    let new_util_pos = find_min min u2 |> snd in
    move_new new_util_pos player

(*[chance_mv] handles moving [player] according to the instructions in the cad
  [c]*)
let chance_mv c player =
  let new_loc = c.contents.actions.move in
  if new_loc = "Boardwalk" then player.board_position <- 39
  else if new_loc = "Go" then begin
    player.board_position <- 0;
    add_money player 200
  end
  else if new_loc = "Illinois Avenue" then move_new 24 player
  else if new_loc = "St. Charles Place" then move_new 11 player
  else if new_loc = "Reading Railroad" then move_new 5 player
  else if new_loc = "Railroad" || new_loc = "Utility" then
    chance_rail_util new_loc player
  else if new_loc = "Back 3" then
    player.board_position <- player.board_position - 3

(**[chance_bal] handles updating the balance of [player] according to the
   instructions on the chance card [c]*)
let chance_bal c player =
  let bal = c.contents.actions.balance_change in
  if bal < 0 then deduct_money player (bal * -1) else add_money player bal

(**[chance_jail] moves[player] to jail and updates the player fields (in_jail
   and board_position) accordingly*)
let chance_jail player =
  player.board_position <- 30;
  player.in_jail <- true

(**[_chance_action ] handles the different actions on the Chance card [_c] on
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

(**[_cc_action] will perform the designated action given in the community chance
   card [c] on [player]*)
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

(**[_handle_cc] prints the text on the head of the Community Chest list*)
let _handle_cc player =
  let head = List.hd !cc_lst in
  print_endline (card_display_info head);
  _cc_action head player

(**[_handle_chance] prints the text on the head of the Chance card list*)
let _handle_chance player =
  let hd = List.hd !chance_lst in
  print_endline (card_display_info hd);
  _chance_action hd player

(**[handle_card] decides whether the position that the player landed in was for
   Community Chest or Chance*)
let handle_card loc player =
  let tile_name = get_tile_name loc monopoly_list in
  if tile_name = "Community Chest" then _handle_cc player
  else if tile_name = "Chance" then _handle_chance player

(**[handle_move] decides whether the player landed on a location where they draw
   a card or on some other type of tile*)
let handle_move loc player =
  let card_locs = [ 2; 7; 17; 22; 33; 36 ] in
  if List.mem loc card_locs then handle_card loc player
  else check_property player.board_position player

let print_turn_info frst scnd player =
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
        update_game_data shuffled_players;
        print_endline "Continue playing? y/n/end game";

        let cont = read_line () in
        if cont = "y" then play_loop cont shuffled_players
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
