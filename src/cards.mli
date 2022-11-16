type card = {
  name : string;
  flavor_text : string;
}

type t = Yojson.Basic.t

val to_json : string -> t
val init_card : string -> string -> card
val card_display_info : card -> string
val card_name : t * t -> string
val card_flvr_txt : t * t -> string
