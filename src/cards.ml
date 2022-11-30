open Yojson.Basic.Util

(* source material: https://monopoly.fandom.com/wiki/Community_Chests*)

(* TODO: adjust type card so can store actions*)
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

(*******************************************************************************
  Helper functions for Card Tests
  *****************************************************************************)

(*******************************************************************************
  End helper functions for Card Tests
  *****************************************************************************)

(*******************************************************************************
  Functions that deal with JSON
  *****************************************************************************)

let actions j =
  {
    move = j |> member "move" |> to_string;
    pay = j |> member "pay" |> to_string;
    receive = j |> member "receive" |> to_string;
  }

let contents j =
  {
    name = j |> member "name" |> to_string;
    flavor_text = j |> member "flavor text" |> to_string;
    actions = j |> member "actions" |> actions;
  }

let card j =
  {
    card_type = j |> member "card type" |> to_string;
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

let card_display_info (crd : card) =
  "Picked up card " ^ crd.contents.name ^ ": " ^ crd.contents.flavor_text
(*******************************************************************************
  End Functions that DONT deal with JSON
  *****************************************************************************)
