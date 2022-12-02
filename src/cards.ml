open Yojson.Basic.Util

(* source material: https://monopoly.fandom.com/wiki/Community_Chests*)

(* TODO: adjust type card so can store actions*)
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

let actions j =
  {
    move = j |> member "move" |> to_int;
    money_change = j |> member "receive" |> to_int;
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
let init_contents nm flvr_txt acts =
  { name = nm; flavor_text = flvr_txt; actions = acts }

let init_card ct nm flvr_txt acts =
  { card_type = ct; contents = init_contents nm flvr_txt acts }

(* TODO: function/s that execute the action/s of the card*)
let find_chance card =
  match card with
  | Chance, _ -> true
  | _ -> failwith "Impossible"

let find_cc card =
  match card with
  | CC, _ -> true
  | _ -> failwith "Impossible"

let make_chance_list cd = List.filter find_chance cd
let make_cc_list cd = List.filter find_cc cd

let card_display_info (crd : card) =
  "Picked up card " ^ crd.contents.name ^ ": " ^ crd.contents.flavor_text
(*******************************************************************************
  End Functions that DONT deal with JSON
  *****************************************************************************)
