type _balance = Bank._balance
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

(*****************************************************************************)
(*********************** Player Helper Functions ******************************)
let update_balance f p i =
  let old_balance = p.balance in
  let new_balance = f old_balance i in
  p.balance <- new_balance

let remove_jailed p = p.in_jail <- false
let remove_free_jail_card p = p.free_jail <- false
let handle_jail_doubles player = if player.in_jail then remove_jailed player

let rec remove_player player player_list =
  match player_list with
  | [] -> failwith "how are you playing with an empty player list"
  | [ _ ] -> []
  | h :: t ->
      if h != player then h :: remove_player player t
      else remove_player player t

(* TODO: Remove -- disable warning temporarily *)
let _ = remove_player

let enqueue_player (players : _player list) (new_player : _player) =
  players @ [ new_player ]

let dequeue_player (players : _player list) : _player * _player list =
  match players with
  | [] -> failwith "no players in list"
  | h :: t -> (h, t)

(*****************************************************************************)
(******************************* Player Functions ******************************)
let init_player nm =
  if nm = "" then failwith "empty string"
  else
    let open Bank in
    {
      board_position = 0;
      name = nm;
      balance = init_balance ();
      doubles = 0;
      free_jail = false;
      in_jail = false;
      jailstats = { turns_since = 0 };
    }

let get_board_position player = player.board_position
let get_name player = player.name

let add_money p i =
  let open Bank in
  update_balance add_to_balance p i

let deduct_money p i =
  let open Bank in
  update_balance deduct_from_balance p i

let set_board_position player pos = player.board_position <- pos

let shuffle_player (players : _player list) =
  let new_player_up, other_players = dequeue_player players in
  (new_player_up, enqueue_player other_players new_player_up)

(*****************************************************************************)
(********************** Player + Jail Functions ***********************)

let charge_jail_fine player =
  deduct_money player 50;
  remove_jailed player;
  player.jailstats.turns_since <- 0;
  player

let check_for_double player roll1 roll2 =
  if roll1 = roll2 then player.doubles <- player.doubles + 1;
  handle_jail_doubles player

let rec handle_free_jail_card (player : _player) =
  let _ =
    print_endline
      "You have a get out of jail free card. Would you like to use it? y/n"
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

let rec handle_jail_fine (player : _player) =
  let _ = print_endline "Would you like to pay the 50$ fee to leave? y/n" in
  let fine_resp = read_line () in
  match fine_resp with
  | "y" -> charge_jail_fine player
  | "n" -> player
  | "" | _ ->
      print_endline "Please provide a valid response: y/n";
      handle_jail_fine player

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

(******************************************************************************)
(******************** Player Tests Helper Function  ***********************)

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

(******************************************************************************)