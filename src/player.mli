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

val make_balance :
  int -> int -> int -> int -> int -> int -> int -> int -> _balance

val make_player : int -> string -> _balance -> int -> _player
val init_player : string -> _player
val get_board_position : _player -> int
val get_name : _player -> string
val set_board_position : _player -> int -> unit

val update_balance : _player -> _balance -> _player
(** [update_balance player bal] returns a new player with the balance [bal] *)

val enqueue_player : _player list -> _player -> _player list
(** [enqueue_player playerlist] enqueues a player and returns a tuple with the
    dequeued player and list of remaining players *)

val dequeue_player : _player list -> _player * _player list
(** [dequeue_player playerlist] dequeues a player and returns a tuple with the
    dequeued player and list of remaining players *)

val shuffle_player : _player list -> _player * _player list
(** [shuffle_player playerlist] returns a tuple with the current player and list of upcoming players *)
val check_jail_status : _player -> _player
(** [shuffle_player playerlist] returns a tuple with the current player and list
    of upcoming players *)

val distribute_change: _player ->int ->unit
val decrement_balance: _player ->int ->unit
