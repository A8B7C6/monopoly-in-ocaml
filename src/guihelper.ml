(* open Player

   let current_id = ref 0

   let write_game_data json_str = Printf.fprintf (open_out
   "../gui/data/game2.json") json_str

   type game_data_player = { id : string; name : string; balance : int; position
   : int; }

   type game_data = { gameStarted : bool; players : game_data_player list;
   current_player : int; lastDiceRoll : int; }

   let format_game_data_player (player : _player) = current_id := !current_id +
   1; { id = string_of_int !current_id; name = player.name; balance =
   player.balance.total; position = player.board_position; }

   let _ = current_id let _ = format_game_data_player let _ = write_game_data *)
