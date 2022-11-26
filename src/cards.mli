type actions = {
  move : string;
  pay : string;
  receive : string;
}

type card = {
  name : string;
  flavor_text : string;
  actions : actions;
}

type t = Yojson.Basic.t

val card_list : t -> card list
val init_card : string -> string -> actions -> card

val card_display_info : card -> string
(** [card_display_info crd] takes [crd] and returns a string detailing what card
    was picked up and what to expect from it. *)
