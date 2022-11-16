open Player

val roll_dice : unit -> int
val print_board : unit -> unit

val do_turn : _player -> _player
(**takes a player type and returns a new player with the updated board position
   after a dice roll *)
