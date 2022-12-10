val roll_dice : unit -> int
(** [roll_dice ()] simulates a single dice roll. Returns an int.*)

val monopoly_list : (int * Locations.tile_type) list
(** [monopoly] returns the data from Monopoly.json in accessible format*)

val do_turn : int -> int -> Player._player -> unit
(** [do_turn r1 r2 p] takes the rolls [r1] and [r2] for the player [p] to change
    the player's board position *)

val list_of_cards_json : Cards.card list
