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
