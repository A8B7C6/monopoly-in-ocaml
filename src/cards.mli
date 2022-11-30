type actions = {
  move : string;
  pay : string;
  receive : string;
}

type contents = {
  name : string;
  flavor_text : string;
  actions : actions;
}

type card = {
  card_type : string;
  contents : contents;
}

type card_deck = { card_deck : card list }

type card_type =
  | Chance of card
  | CC of card

type t = Yojson.Basic.t

val parse : t -> card list
val init_card : string -> string -> string -> actions -> card

val card_display_info : card -> string
(** [card_display_info crd] takes [crd] and returns a string detailing what card
    was picked up and what to expect from it. *)
