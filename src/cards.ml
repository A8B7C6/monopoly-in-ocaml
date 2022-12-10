open Yojson.Basic.Util

(* source material: https://monopoly.fandom.com/wiki/Community_Chests*)

(* TODO: adjust type card so can store actions*)
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

(*******************************************************************************
  Helper functions for Card Tests
  *****************************************************************************)

(*******************************************************************************
  End helper functions for Card Tests
  *****************************************************************************)

(*******************************************************************************
  Functions that deal with JSON
  *****************************************************************************)
let bool_helper s =
  match s with
  | "true" -> true
  | "false" -> false
  | _ -> failwith "Impossible"

let int_helper s : int = int_of_string s

let actions j =
  {
    move = j |> member "move" |> to_string;
    balance_change = j |> member "receive" |> to_string |> int_helper;
    go_to_jail = j |> member "go to jail" |> to_string |> bool_helper;
    out_of_jail_card =
      j |> member "get out of jail free card" |> to_string |> bool_helper;
  }

let contents j =
  {
    name = j |> member "name" |> to_string;
    flavor_text = j |> member "flavor text" |> to_string;
    actions = j |> member "actions" |> actions;
  }

let type_helper ct =
  match ct with
  | "chance" -> Chance
  | "cc" -> CC
  | _ -> failwith "Impossible"

let card j =
  {
    card_type = j |> member "card type" |> to_string |> type_helper;
    contents = j |> member "contents" |> contents;
  }

let card_deck j = j |> member "card list" |> to_list |> List.map card

let parse j =
  try card_deck j with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

(*******************************************************************************
  End Functions that deal with JSON
  *****************************************************************************)

(*******************************************************************************
  Functions that DONT deal with JSON
  *****************************************************************************)
let init_actions mv rcv gtj ooj =
  { move = mv; balance_change = rcv; go_to_jail = gtj; out_of_jail_card = ooj }

let init_contents nm flvr_txt mv rcv gtj ooj =
  { name = nm; flavor_text = flvr_txt; actions = init_actions mv rcv gtj ooj }

let init_card ct nm flvr_txt mv rcv gtj ooj =
  { card_type = ct; contents = init_contents nm flvr_txt mv rcv gtj ooj }

(* TODO: function/s that execute the action/s of the card*)
let find_chance (cd : card) =
  match cd with
  | { card_type = Chance; _ } -> true
  | _ -> false

let find_cc (cd : card) =
  match cd with
  | { card_type = CC; _ } -> true
  | _ -> false

let make_chance_list cdl = List.filter find_chance cdl
let make_cc_list cdl = List.filter find_cc cdl
let card_json = Yojson.Basic.from_file "src/data/Cards.json"
let chance_lst = make_chance_list (parse card_json)
let cc_lst = make_chance_list (parse card_json)
let _ = chance_lst @ cc_lst

let card_display_info (crd : card) =
  "Picked up card " ^ crd.contents.name ^ ": " ^ crd.contents.flavor_text
(*******************************************************************************
  End Functions that DONT deal with JSON
  *****************************************************************************)
