(** Functionality for a Player's Funds

    An extraction of a Player's funds due to Player verbosity. It handles
    tracking, adding, and subtracting bills from a balance. The results are used
    in regards to the field Player._player.balance *)

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
(**[_balance] is the type for all of the player balances*)

val init_balance : unit -> _balance
(** [init_balance] creates a starting balance of [1500] *)

val add_to_balance : _balance -> int -> _balance
(** [add_to_balance b i] adds [i] dollars to the balance [b]*)

val deduct_from_balance : _balance -> int -> _balance
(** [deduct_from_balance b i] removes [i] dollars from the balance [b]*)

(* Helper Functions for Bank Tests*)
val make_balance :
  int -> int -> int -> int -> int -> int -> int -> int -> _balance
(** [make_balance total fh h ft tt t f o] creates a balance that has the total
    [total]. Requires:[total] =
    [fh * 500 + h *100 + ft * 50 + tt * 20 + t*10 + f*5 + o]*)
