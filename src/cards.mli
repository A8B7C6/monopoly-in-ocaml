type actions = {
  move : string;
  balance_change : int;
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

val init_card :
  card_type -> string -> string -> string -> int -> bool -> bool -> card

val make_chance_list : card list -> card list
val make_cc_list : card list -> card list

val card_display_info : card -> string
(** [card_display_info crd] takes [crd] and returns a string detailing what card
    was picked up and what to expect from it. *)

val chance_lst : card list ref
val cc_lst : card list ref

val to_bottom : card -> card list ref -> unit
(**[to_bottom crd lst] removes [crd] from the [lst] and appends it to the back
   of the list*)

val remove_jail : card -> card list ref -> unit
(**[remove_jail crd lst] removes the get out of free jail [crd] from the card
   list [lst]*)
