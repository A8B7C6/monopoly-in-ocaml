open Yojson.Basic.Util

(* source material: https://monopoly.fandom.com/wiki/Community_Chests*)

(* TODO: adjust type card so can store actions*)
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
    pay = j |> member "pay bank" |> to_string;
    receive = j |> member "receive bank" |> to_string;
  }

let card j =
  {
    name = j |> member "name" |> to_string;
    flavor_text = j |> member "flavor text" |> to_string;
    actions = actions j;
  }

let card_list j = j |> member "cards" |> to_list |> List.map card

(*******************************************************************************
  End Functions that deal with JSON
  *****************************************************************************)

(*******************************************************************************
  Functions that DONT deal with JSON
  *****************************************************************************)

let init_card nm flvr_txt acts =
  { name = nm; flavor_text = flvr_txt; actions = acts }

(* TODO : need to build card_list from Cards.json data*)

(* TODO: function/s that execute the action/s of the card*)

let card_display_info crd =
  "Picked up card " ^ crd.name ^ ": " ^ crd.flavor_text
(*******************************************************************************
  End Functions that DONT deal with JSON
  *****************************************************************************)
