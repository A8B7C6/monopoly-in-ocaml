(** Supposed to Manage the State of the Game

    most of the functionality that would go here is currently in bin/main.ml.
    Organizational goal would be to move some functionality from there to here. *)

val roll_dice : unit -> int
(** [roll_dice ()] simulates a single dice roll. Returns a randomized int in the
    range [1-6]*)

val do_turn : int -> int -> Player._player -> unit
(** [do_turn r1 r2 p] takes the rolls [r1] and [r2] to determine the player
    [p]'s new board position *)
