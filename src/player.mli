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

val make_balance :
  int -> int -> int -> int -> int -> int -> int -> int -> _balance

val make_player : int -> string -> _balance -> int -> _player
val init_player : string -> _player
val get_board_position : _player -> int
val get_name : _player -> string
