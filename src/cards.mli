type card = {
  name : string;
  flavor_text : string;
}

type t = Yojson.Basic.t

val init_card : string -> string -> card

val card_display_info : card -> string
(** [card_display_info crd] takes [crd] and returns a string detailing what card
    was picked up and what to expect from it. *)
