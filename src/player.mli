type _player = {
  mutable board_position : int;
  name : string;
}

val init_player : string -> _player
val get_board_position : _player -> int
val get_name : _player -> string
