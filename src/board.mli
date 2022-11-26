val roll_dice : unit -> int
(** [roll_dice] simulates a single dice roll. Returns an int.*)

val monopoly_list : (int * Locations.tile_type) list
(** [monopoly] returns the data from Monopoly.json in accessible format*)

val do_turn : Player._player -> Player._player
(** [do_turn p] copies a player [p] and returns a new player with an updated
    board position *)

val list_of_cards_json : Cards.card list
