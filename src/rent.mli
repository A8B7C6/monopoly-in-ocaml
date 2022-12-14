(** Representation of Static Rent Data for Monopoly Locations *)

open Player

val check_property : int -> _player -> unit
(**[check property loc pl] checks if the property given by [loc] is owned. If it
   is owned by [pl] it asks if they would like to upgrade it. If [pl] does not
   own the property, they are asked if they would like to buy it*)
