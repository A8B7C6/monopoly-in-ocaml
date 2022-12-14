(** Representation of Static Monopoly Chance Cards and Community Chest Cards

    The module represents the information stored in data/Cards.json. Handles
    loading of data and querying the data.*)

type actions = {
  move : string;
  balance_change : int;
  go_to_jail : bool;
  out_of_jail_card : bool;
}
(**[actions] is the type that holds the information related to which moves the
   card has the player do*)

type contents = {
  name : string;
  flavor_text : string;
  actions : actions;
}
(**[contents] is the type that holds the name of the card, the description, and
   the actions that must be perfomed as given on the card*)

type card_type =
  | Chance
  | CC

(**[card_type] is whether the card is a Chance or Community Chest card*)

type card = {
  card_type : card_type;
  contents : contents;
}
(**[card] is the type that holds the information of what type of [card] this is
   and the [contents]*)

type card_deck = { card_deck : card list }
(**[card_deck] is the type that represents the entire stack of cards*)

type t = Yojson.Basic.t
(**[t] is the abstract type of json files*)

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
