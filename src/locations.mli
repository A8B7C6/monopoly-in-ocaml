type property
type tile_type

val to_json : string -> Yojson.Basic.t
val loc_contents : Yojson.Basic.t -> Yojson.Basic.t
val property_record : Yojson.Basic.t -> property
