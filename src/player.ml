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

type _player = {
  mutable board_position : int;
  name : string;
  balance : _balance;
  mutable doubles : int;
  free_jail : bool;
  mutable in_jail : bool;
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
  }

let get_name player = player.name
let get_board_position player = player.board_position
let set_board_position player pos = player.board_position <- pos

let update_balance player new_bal =
  {
    board_position = player.board_position;
    name = player.name;
    balance = new_bal;
    doubles = player.doubles;
    free_jail = false;
    in_jail = false;
  }

let rec remove_player players player_name =
  match players with
  | [] -> []
  | h :: t ->
      if h.name != player_name then h :: remove_player t player_name
      else remove_player t player_name

let enqueue_player (players : _player list) (new_player : _player) =
  players @ [ new_player ]

let dequeue_player (players : _player list) : _player * _player list =
  match players with
  | [] -> failwith "no players in list"
  | h :: t -> (h, t)

let shuffle_player (players : _player list) =
  let new_player_up, other_players = dequeue_player players in
  (new_player_up, enqueue_player other_players new_player_up)
