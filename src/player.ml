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
}

(*******************************************************************************
  Helper functions for Player Tests
  *****************************************************************************)
let make_balance total fivehun hun ffty twnty tens fives ones =
  { total; fivehun; hun; ffty; twnty; tens; fives; ones }

let make_player brdpos nm blnce dbls =
  { board_position = brdpos; name = nm; balance = blnce; doubles = dbls }

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
  { board_position = 0; name = nm; balance = init_balance; doubles = 0 }

let get_name player = player.name
let get_board_position player = player.board_position

(* TODO: function that updates the balance of a player. should take in a player
   and return a player*)

(* TODO: function that takes in a list of players and REMOVES one of them.
   REquires: order of players maintained*)

(* TODO: function that takes in a list of players and ADDS one of them.
   Requires: order of the list is maintained and the player is added to the end
   of the list *)
