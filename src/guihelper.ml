open Player

let current_id = ref 0

type game_data_player = {
  id : string;
  name : string;
  balance : int;
  position : int;
  last_dice_roll : int;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type game_data = {
  gameStarted : bool;
  players : game_data_player list;
  current_player : string;
  lastDiceRoll : int;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

let format_game_data_player (player : _player) =
  current_id := !current_id + 1;
  {
    id = string_of_int !current_id;
    name = player.name;
    balance = player.balance.total;
    position = player.board_position;
    last_dice_roll = player.last_dice_roll;
  }

let create_game_data (players : _player list) =
  let rec format_players lst =
    match lst with
    | [] -> []
    | h :: t -> format_game_data_player h :: format_players t
  in
  let formatted_players = format_players players in
  let game_data =
    {
      gameStarted = true;
      players = formatted_players;
      current_player = (List.hd formatted_players).name;
      lastDiceRoll = (List.hd formatted_players).last_dice_roll;
    }
  in
  game_data

let update_game_data (players : _player list) =
  let game_data = create_game_data players in
  let json_file = "./gui/data/game.json" in
  let yo_json = game_data_to_yojson game_data in
  Yojson.Safe.to_file json_file yo_json
