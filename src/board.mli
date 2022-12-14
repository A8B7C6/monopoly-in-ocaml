(** Functionality for a player's movement on the board *)

val roll_dice : unit -> int
(** [roll_dice ()] simulates a single dice roll. Returns a randomized int in the
    range [1-6]*)

val do_turn : int -> int -> Player._player -> unit
(** [do_turn r1 r2 p] takes the rolls [r1] and [r2] to determine the player
    [p]'s new board position *)

val move_new : int -> Player._player -> unit
(** [move_new new_index player] handles moving [player] to new positions at
    [new_index] which is given by the chance cards, and handles their balance if
    they pass Go*)

val chance_mv : Cards.card -> Player._player -> unit
(* [chance_mv c player] handles moving [player] according to the instructions in
   the cad [c]*)

val find_min : 'a -> ('a * 'b) list -> 'a
(** [find_min min lst] returns the second smallest element [min] in [lst].
    Comparisons are done on the first element of the tuples. Function is used as
    a helper in finding the closest Railroad or Utility to which a player has to
    move when drawing a chance card.contents Requires: [lst] is sorted in
    ascending order by the first element*)
