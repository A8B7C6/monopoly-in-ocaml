type _player = {
  mutable board_position : int;
  name : string;
}

let init_player nm = { board_position = 0; name = nm }
let get_name player = player.name
let get_board_position player = player.board_position
