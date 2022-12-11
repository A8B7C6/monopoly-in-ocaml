open Player

val update_game_data : _player list -> unit
(**[update_game_data pl] takes the list of players [pl] and creates a JSON file
   [gui/data/game.json] that tracks player information.*)
