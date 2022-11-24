(* open Yojson.Basic.Util *)

(* source material: https://monopoly.fandom.com/wiki/Community_Chests*)

(* TODO: adjust type card so can store actions*)
type card = {
  name : string;
  flavor_text : string;
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

(* let card_name j = j |> fst |> to_string let card_flvr_txt j = j |> snd |>
   member "flavor text" |> to_string *)

(* TODO: function/s that parse the actions of a card*)

(*******************************************************************************
  End Functions that deal with JSON
  *****************************************************************************)

(*******************************************************************************
  Functions that DONT deal with JSON
  *****************************************************************************)

let init_card nm flvr_txt = { name = nm; flavor_text = flvr_txt }

(* TODO : need to build card_list from Cards.json data*)

(* TODO: function/s that execute the action/s of the card*)

let card_display_info crd =
  "Picked up card " ^ crd.name ^ ": " ^ crd.flavor_text
(*******************************************************************************
  End Functions that DONT deal with JSON
  *****************************************************************************)
