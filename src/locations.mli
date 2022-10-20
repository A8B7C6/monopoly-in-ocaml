type _property

type tile_type =
  | Property of _property
  | Go
  | CommunityChest
  | Chance
  | Jail

val to_json : string -> Yojson.Basic.t
val loc_contents : Yojson.Basic.t -> Yojson.Basic.t
val make_contents : string -> int -> tile_type
val make_tile : int -> string -> string -> int -> int * tile_type
val property_record : Yojson.Basic.t -> _property

val parse_tile_type : Yojson.Basic.t -> tile_type
(** [parse_tile_type tile] takes a tile, parse the tile into an approriate
    record, and returns tile_type. Requires: a valid tile*)

val tiles_list : Yojson.Basic.t -> (int * tile_type) list
(** [tiles_list json] takes json representations of a monopoly board and returns
    a list of locations where each tile is associated with its index*)

val find_tile : int -> (int * tile_type) list -> tile_type
(** [find_tile index tiles] looks within the list of tiles to find the tile
    associated with [index]*)
