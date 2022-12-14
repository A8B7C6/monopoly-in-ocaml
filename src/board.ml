open Player
open Cards

let roll_dice () =
  let state = Random.State.make_self_init () in
  Random.State.int state 6 + 1

let do_turn (frst : int) (scnd : int) (player : _player) =
  let total = frst + scnd in
  if total + player.board_position >= 40 then
    set_board_position player (total + player.board_position - 40)
  else set_board_position player (total + player.board_position)

let move_new new_index player =
  if player.board_position > new_index then begin
    player.board_position <- new_index;
    add_money player 200
  end
  else player.board_position <- new_index

let rec find_min min lst =
  match lst with
  | [] -> min
  | [ (a, _) ] -> a
  | (a, _) :: (b, c) :: t -> if a = min then b else find_min min ((b, c) :: t)

let chance_rail_util loc player =
  let pos = player.board_position in
  let railroads = [ 5; 15; 25; 35 ] in
  let utilities = [ 12; 28 ] in
  if loc = "Railroad" then
    let r1 = List.sort_uniq compare (pos :: railroads) in
    let r2 = List.map (fun x -> (x, 0)) r1 in
    if pos > 35 then move_new 5 player
    else
      let r_pos = find_min pos r2 in
      move_new r_pos player
  else if loc = "Utility" then
    let u1 = List.sort_uniq compare (pos :: utilities) in
    let u2 = List.map (fun x -> (x, 0)) u1 in
    if pos > 28 then move_new 12 player
    else
      let u_pos = find_min pos u2 in
      move_new u_pos player

let chance_mv c player =
  let new_loc = c.contents.actions.move in
  if new_loc = "Boardwalk" then player.board_position <- 39
  else if new_loc = "Illinois Avenue" then move_new 24 player
  else if new_loc = "St. Charles Place" then move_new 11 player
  else if new_loc = "Reading Railroad" then move_new 5 player
  else if new_loc = "Railroad" || new_loc = "Utility" then
    chance_rail_util new_loc player
  else if new_loc = "Back 3" then
    player.board_position <- player.board_position - 3
  else if new_loc = "Go" then begin
    player.board_position <- 0;
    add_money player 200
  end
