open Player

let roll_dice () =
  let _ = Random.self_init in
  Random.int 6 + 1

let do_turn (frst : int) (scnd : int) (player : _player) =
  let total = frst + scnd in
  if total + player.board_position >= 40 then
    set_board_position player (40 - (total + player.board_position))
  else set_board_position player (total + player.board_position)
