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

val init_balance : unit -> _balance
(** [init_balance] creates a starting balance of [1500] *)

val add_to_balance : _balance -> int -> _balance
val deduct_from_balance : _balance -> int -> _balance

val make_balance :
  int -> int -> int -> int -> int -> int -> int -> int -> _balance
