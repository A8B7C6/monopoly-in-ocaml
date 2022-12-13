(** Representation of Static Monopoly Chance Cards and Community Chest Cards

    The module represents the information stored in data/Cards.json. Handles
    loading of data and querying the data.*)

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
(** [parse t] takes information from a json file [t] and generates a [card list]
    with all of the [card_type] types*)

val init_card :
  card_type -> string -> string -> string -> int -> bool -> bool -> card
(** [init_card ct nm flvr_txt mv rcv gtj ooj] creates a card of card_type [ct]*)

val make_chance_list : card list -> card list
(** [make_chance_list p] takes a card list [p] with multiple card types ad
    returns a list with only the card type [Chance] *)

val make_cc_list : card list -> card list
(** [make_cc_list p] takes a card list [p] with multiple card types ad returns a
    list with only the card type [CC] *)

val card_display_info : card -> string
(** [card_display_info crd] takes [crd] and returns a string detailing what card
    was picked up and what to expect from it. *)

val chance_lst : card list ref
(** [chance_lst] is a mutable instance that holds a list of cards with type
    [Chance]*)

val cc_lst : card list ref
(** [cc_list] is a mutable instance that holds a list of cards with type [CC]*)

val to_bottom : card -> card list ref -> unit
(**[to_bottom crd lst] removes [crd] from the [lst] and appends it to the back
   of the list*)

val remove_jail : card -> card list ref -> unit
(**[remove_jail crd lst] removes the get out of free jail [crd] from the card
   list [lst]*)