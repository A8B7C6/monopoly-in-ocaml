open Yojson.Basic.Util

(* source material: https://monopoly.fandom.com/wiki/Community_Chests*)
type card = {
  name : string;
  flavor_text : string;
}

type t = Yojson.Basic.t

let init_card nm flvr_txt = { name = nm; flavor_text = flvr_txt }

(* FIX : think we should move this function so we dont have it written twice, as
   of rn is written once here and once in locations *)
let to_json json = Yojson.Basic.from_file json
let card_name j = j |> fst |> to_string
let card_flvr_txt j = j |> snd |> member "flavor text" |> to_string

(* BUILD : need to build card_list, have started it: let card_list j = let data
   = to_assoc j in data*)

let card_display_info crd =
  "Picked up card " ^ crd.name ^ ": " ^ crd.flavor_text
