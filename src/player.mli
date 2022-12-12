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

val init_player : string -> _player
(** [init_player nm] creates a player with name [nm], starting position at index
    0, and starting balance of [1500] *)

val get_board_position : _player -> int
(** [get_board_position p] gives the [board_position] of player [p]*)

val get_name : _player -> string
(** [get_name p] gives the [name] of player [p]*)

val add_money : _player -> int -> unit
(** [add_money p i] adds [i] dollars to the balance of player [p]*)

val deduct_money : _player -> int -> unit
(** [deduct_money p i] removes [i] dollars from the balance of player [p]*)

val set_board_position : _player -> int -> unit
(** [set_board_position p i] sets the [board_position] of player [p] to be at
    index [i]*)

val shuffle_player : _player list -> _player * _player list
(** [shuffle_player playerlist] is a pair [(p, pl)]. [p] is the element at the
    head of [playerlist], [pl] is the tail of [playerlist] appended to [\[p\]].
    Throws an exception if [playerlist] is empty *)

val check_for_double : _player -> int -> int -> unit
(** [check_for_double p r1 r2] moniters the number of doubles rolled by a
    player. *)

val check_jail_status : _player -> _player
(** [shuffle_player playerlist] returns a tuple with the current player and list
    of upcoming players *)

(*Helper functions for tests*)

val make_player : int -> string -> _balance -> int -> _player
