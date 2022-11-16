(* Helper method utilized to simulate a single dice roll. Returns a number
   utilizing built-in Random module, reseeds the generator with the current UNIX
   time.*)
let roll_dice () =
  let _ = Random.init (int_of_float (Unix.gettimeofday ())) in
  Random.int 11 + 2 (*dice roll ranges from 2 to 12*)