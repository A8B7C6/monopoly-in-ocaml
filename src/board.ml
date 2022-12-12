open Player

let roll_dice () =
  let state = Random.State.make_self_init () in
  Random.State.int state 6 + 1

let do_turn (frst : int) (scnd : int) (player : _player) =
  let total = frst + scnd in
  if total + player.board_position >= 40 then
    set_board_position player (total + player.board_position - 40)
  else set_board_position player (total + player.board_position)
