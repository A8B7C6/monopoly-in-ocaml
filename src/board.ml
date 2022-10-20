(* Helper method utilized to simulate a single dice roll. Returns a number
   utilizing built-in Random module, reseeds the generator with the current UNIX
   time.*)
let roll_dice () =
  let _ = Random.init (int_of_float (Unix.gettimeofday ())) in
  Random.int 5 + 1

(** [do_turn player] takes a player type and returns a new player with the
    updated board position after a dice roll *)
(* let do_turn (player : Player.player) : Player.player = (* before this we
   should output the player's turn has begun *) let dice1 = roll_dice () in let
   dice2 = roll_dice () in let total_movement = dice1 + dice2 in (* before this
   we should output the player's invidiual dice rolls & then increment their
   position *) let board_position = (get_board_position player) + total_movement
   in (* here we should communicate the new tile the player is on & handle any
   effects of landing on that new tile *) *)
