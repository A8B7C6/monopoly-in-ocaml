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

(** [find_min min lst] returns the smallest element [min] in [lst]. Comparisons
    are done on the second element of the tuples. Function is used as a helper
    in finding the closest Railroad or Utility to which a player has to move
    when drawing a chance card*)
let rec find_min min lst =
  match lst with
  | [] -> !min
  | (a, b) :: t ->
      if b < snd !min then begin
        min := (a, b);
        find_min min t
      end
      else find_min min t

(** [chance_rail_util loc player] will move [player] to the nearst board
    position [loc] that has type 'Railroad' or 'Utilities'. Function is used to
    intitiate the player's move to the nearest board pos given by chance card *)
let chance_rail_util loc player =
  let pos = player.board_position in
  let railroads = [ 5; 15; 25; 35 ] in
  let utilities = [ 12; 28 ] in
  if loc = "Railroad" then
    let r2 = List.map (fun x -> (x, Int.abs (x - pos))) railroads in
    let min_ref = ref (List.hd r2) in
    let new_board_pos = find_min min_ref r2 in
    move_new (snd new_board_pos) player
    (*have to check if railroad is owned and if have to pay price*)
  else if loc = "Utilities" then
    let u2 = List.map (fun x -> (x, Int.abs (x - pos))) utilities in
    let min = ref (List.hd u2) in
    let new_util_pos = find_min min u2 |> snd in
    move_new new_util_pos player

let chance_mv c player =
  let new_loc = c.contents.actions.move in
  if new_loc = "Boardwalk" then player.board_position <- 39
  else if new_loc = "Go" then begin
    player.board_position <- 0;
    add_money player 200
  end
  else if new_loc = "Illinois Avenue" then move_new 24 player
  else if new_loc = "St. Charles Place" then move_new 11 player
  else if new_loc = "Reading Railroad" then move_new 5 player
  else if new_loc = "Railroad" || new_loc = "Utility" then
    chance_rail_util new_loc player
  else if new_loc = "Back 3" then
    player.board_position <- player.board_position - 3
