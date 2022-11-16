open Player

(* Helper method utilized to simulate a single dice roll. Returns a number
   utilizing built-in Random module, reseeds the generator with the current UNIX
   time.*)
let roll_dice () =
  let _ = Random.init (int_of_float (Unix.gettimeofday ())) in
  Random.int 6 + 1

let do_turn player =
  print_endline (player.name ^ ", your turn has begun");
  let frst = roll_dice () in
  let scnd = roll_dice () in
  print_endline
    ("You rolled a " ^ string_of_int frst ^ " and a " ^ string_of_int scnd);
  { player with board_position = (player.board_position + frst + scnd) mod 40 }

(* let do_turn (player : Player.player) : Player.player = (* before this we
   should output the player's turn has begun *) let dice1 = roll_dice () in let
   dice2 = roll_dice () in let total_movement = dice1 + dice2 in (* before this
   we should output the player's invidiual dice rolls & then increment their
   position *) let board_position = (get_board_position player) + total_movement
   in (* here we should communicate the new tile the player is on & handle any
   effects of landing on that new tile *) *)
