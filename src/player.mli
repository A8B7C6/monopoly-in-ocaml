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

val init_balance : _balance
(** [init_balance] creates a starting balance of [1500] *)

val init_player : string -> _player
(** [init_player nm] creates a player with name [nm], starting position at index
    0, and starting balance of [1500] *)

val get_board_position : _player -> int
(** [get_board_position p] gives the [board_position] of player [p]*)

val get_name : _player -> string
(** [get_name p] gives the [name] of player [p]*)

val set_board_position : _player -> int -> unit
(** [set_board_position p i] sets the [board_position] of player [p] to be at
    index [i]*)

val distribute_change : _player -> int -> unit
val decrement_balance : _player -> int -> unit

val shuffle_player : _player list -> _player * _player list
(** [shuffle_player playerlist] is a pair [(p, pl)]. [p] is the element at the
    head of [playerlist], [pl] is the tail of [playerlist] appended to [\[p\]].
    Throws an exception if [playerlist] is empty *)

val check_jail_status : _player -> _player
(** [shuffle_player playerlist] returns a tuple with the current player and list
    of upcoming players *)

val check_for_double : _player -> int -> int -> unit

(*Helper functions for tests*)

val make_player : int -> string -> _balance -> int -> _player

val make_balance :
  int -> int -> int -> int -> int -> int -> int -> int -> _balance

val update_balance : _player -> _balance -> _player
(** [update_balance player bal] updates a player [p] to have balance [bal] *)
