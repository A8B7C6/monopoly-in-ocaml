type actions = {
  move : int;
  money_change : int;
  go_to_jail : bool;
  out_of_jail_card : bool;
}

type contents = {
  name : string;
  flavor_text : string;
  actions : actions;
}

type card_type =
  | Chance
  | CC

type card = {
  card_type : card_type;
  contents : contents;
}

type card_deck = { card_deck : card list }
type t = Yojson.Basic.t

val parse : t -> card list
val init_card : card_type -> string -> string -> actions -> card

val make_chance_list :
  (card_type * contents) list -> (card_type * contents) list

val make_cc_list : (card_type * contents) list -> (card_type * contents) list

val card_display_info : card -> string
(** [card_display_info crd] takes [crd] and returns a string detailing what card
    was picked up and what to expect from it. *)
