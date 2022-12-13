(** Functionality for Connecting OCaml Back End with JS Front End

    Provides functionality to send information from OCaml to a JSON file that is
    read by the JS Front End *)

open Player

val update_game_data : _player list -> unit
(**[update_game_data pl] takes the list of players [pl] and creates a JSON file
   [gui/data/game.json] that tracks player information.*)
