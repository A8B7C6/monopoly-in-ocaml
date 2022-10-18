type property
type tile_type

val to_json : string -> Yojson.Basic.t
val propery_contents : Yojson.Basic.t -> Yojson.Basic.t
val property_record : Yojson.Basic.t -> property
val find_tile : int -> (int * tile_type) list -> tile_type
