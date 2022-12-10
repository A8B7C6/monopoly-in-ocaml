type _balance = {
  mutable total : int;
  mutable fivehun : int;
  mutable hun : int;
  mutable ffty : int;
  mutable twnty : int;
  mutable tens : int;
  mutable fives : int;
  mutable ones : int;
}

type jail_stats = { mutable turns_since : int }

type _player = {
  mutable board_position : int;
  name : string;
  mutable balance : _balance;
  mutable doubles : int;
  mutable free_jail : bool;
  mutable in_jail : bool;
  mutable jailstats : jail_stats;
}

(*******************************************************************************
  Helper functions for Player Tests
  *****************************************************************************)
let make_balance total fivehun hun ffty twnty tens fives ones =
  { total; fivehun; hun; ffty; twnty; tens; fives; ones }

let make_player brdpos nm blnce dbls =
  {
    board_position = brdpos;
    name = nm;
    balance = blnce;
    doubles = dbls;
    free_jail = false;
    in_jail = false;
    jailstats = { turns_since = 0 };
  }

(*******************************************************************************
  End helper functions for Locations Tests
  *****************************************************************************)

let init_balance =
  {
    total = 1500;
    fivehun = 2;
    hun = 2;
    ffty = 2;
    twnty = 6;
    tens = 5;
    fives = 5;
    ones = 5;
  }

let init_player nm =
  {
    board_position = 0;
    name = nm;
    balance = init_balance;
    doubles = 0;
    free_jail = false;
    in_jail = false;
    jailstats = { turns_since = 0 };
  }

let get_name player = player.name
let get_board_position player = player.board_position
let set_board_position player pos = player.board_position <- pos

let update_balance player new_bal =
  player.balance <- new_bal;
  player

let distribute_one player =
  player.balance.ones <- player.balance.ones + 1;
  player.balance.total <- player.balance.total + 1

let distribute_five player =
  player.balance.fives <- player.balance.fives + 1;
  player.balance.total <- player.balance.total + 5

let distribute_ten player =
  player.balance.tens <- player.balance.tens + 1;
  player.balance.total <- player.balance.total + 10

let distribute_twenty player =
  player.balance.twnty <- player.balance.twnty + 20;
  player.balance.total <- player.balance.total + 20

let distribute_fifty player =
  player.balance.ffty <- player.balance.ffty + 1;
  player.balance.total <- player.balance.total + 50

let distribute_hun player =
  player.balance.hun <- player.balance.hun + 1;
  player.balance.total <- player.balance.total + 100

let distribute_five_hundred player =
  player.balance.fivehun <- player.balance.fivehun + 1;
  player.balance.total <- player.balance.total + 500

let deduct_one player =
  player.balance.ones <- player.balance.ones - 1;
  player.balance.total <- player.balance.total - 1

let deduct_five player =
  player.balance.fives <- player.balance.fives - 1;
  player.balance.total <- player.balance.total - 5

let deduct_ten player =
  player.balance.tens <- player.balance.tens - 1;
  player.balance.total <- player.balance.total - 10

let deduct_twenty player =
  player.balance.twnty <- player.balance.twnty - 1;
  player.balance.total <- player.balance.total - 20

let deduct_fifty player =
  player.balance.ffty <- player.balance.ffty - 1;
  player.balance.total <- player.balance.total - 50

let deduct_hundred player =
  player.balance.hun <- player.balance.hun - 1;
  player.balance.total <- player.balance.total - 100

let deduct_five_hundred player =
  player.balance.fivehun <- player.balance.fivehun - 1;
  player.balance.total <- player.balance.total - 500

let rec distribute_change (player : _player) (amt : int) =
  if amt > 500 then
    let _ = distribute_five_hundred player in
    distribute_change player (amt - 500)
  else if amt > 100 then
    let _ = distribute_hun player in
    distribute_change player (amt - 100)
  else if amt > 50 then
    let _ = distribute_fifty player in
    distribute_change player (amt - 50)
  else if amt > 20 then
    let _ = distribute_twenty player in
    distribute_change player (amt - 20)
  else if amt > 10 then
    let _ = distribute_ten player in
    distribute_change player (amt - 10)
  else if amt > 5 then
    let _ = distribute_five player in
    distribute_change player (amt - 5)
  else if amt > 1 then
    let _ = distribute_one player in
    distribute_change player (amt - 1)
  else ()

let rec decrement_balance (player : _player) amt =
  if amt < 0 then distribute_change player (amt * -1)
  else if player.balance.ones > 0 then
    let _ = deduct_one player in
    decrement_balance player (amt - 1)
  else if player.balance.fives > 0 then
    let _ = deduct_five player in
    decrement_balance player (amt - 5)
  else if player.balance.tens > 0 then
    let _ = deduct_ten player in
    decrement_balance player (amt - 10)
  else if player.balance.twnty > 0 then
    let _ = deduct_twenty player in
    decrement_balance player (amt - 20)
  else if player.balance.ffty > 0 then
    let _ = deduct_fifty player in
    decrement_balance player (amt - 50)
  else if player.balance.hun > 0 then
    let _ = deduct_hundred player in
    decrement_balance player (amt - 100)
  else if player.balance.fivehun > 0 then
    let _ = deduct_five_hundred player in
    decrement_balance player (amt - 500)
  else ()

let rec remove_player players player_name =
  match players with
  | [] -> []
  | h :: t ->
      if h.name != player_name then h :: remove_player t player_name
      else remove_player t player_name

(* TODO: Remove -- disable warning temporarily *)
let _ = remove_player

let enqueue_player (players : _player list) (new_player : _player) =
  players @ [ new_player ]

let dequeue_player (players : _player list) : _player * _player list =
  match players with
  | [] -> failwith "no players in list"
  | h :: t -> (h, t)

let shuffle_player (players : _player list) =
  let new_player_up, other_players = dequeue_player players in
  (new_player_up, enqueue_player other_players new_player_up)

let remove_jailed p = p.in_jail <- false
let remove_free_jail_card p = p.free_jail <- false

let rec handle_free_jail_card (player : _player) =
  let _ =
    print_endline
      "You have a get out of jail free card. Would you like to use it? Y/N"
  in
  let card_resp = read_line () in
  match card_resp with
  | "y" ->
      remove_free_jail_card player;
      remove_jailed player;
      player
  | "n" -> player
  | "" | _ ->
      print_endline "Please provide a valid response: y/n";
      handle_free_jail_card player

let charge_jail_fine player =
  decrement_balance player 50;
  remove_jailed player;
  player.jailstats.turns_since <- 0;
  player

let rec handle_jail_fine (player : _player) =
  let _ = print_endline "Would you like to pay the 50$ fee to leave? y/n" in
  let fine_resp = read_line () in
  match fine_resp with
  | "y" -> charge_jail_fine player
  | "n" -> player
  | "" | _ ->
      print_endline "Please provide a valid response: y/n";
      handle_jail_fine player

let handle_jail_doubles player = if player.in_jail then remove_jailed player

let check_for_double player roll1 roll2 =
  if roll1 = roll2 then player.doubles <- player.doubles + 1;
  handle_jail_doubles player

let handle_jailed_player (player : _player) =
  print_endline "You are currently in jail.";
  player.jailstats.turns_since <- player.jailstats.turns_since + 1;
  if player.jailstats.turns_since = 3 then charge_jail_fine player
  else if
    (* If the player has a get out of jail card, give them the ability to use
       it *)
    player.free_jail
  then
    let player = handle_free_jail_card player in
    (* Check if player has used card or is still in jail *)
    if player.in_jail then handle_jail_fine player else player
  else handle_jail_fine player

let check_jail_status (player : _player) =
  if player.in_jail then handle_jailed_player player else player
