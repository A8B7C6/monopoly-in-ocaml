type _property
type _tax

type tile_type =
  | Property of _property
  | Railroad of _property
  | Utility of _property
  | Tax of _tax
  | Go
  | CommunityChest
  | Chance
  | Jail
  | VisitingJail
  | Parking

type color =
  | Brown
  | LightBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | DarkBlue
  | Colorless

val to_json : string -> Yojson.Basic.t

(*val monopoly : Yojson.Basic.t*)
val tile_type : string -> Yojson.Basic.t -> tile_type
val tile_color : string -> color
val tile_index : int -> Yojson.Basic.t -> int * tile_type

val make_tile :
  int ->
  string ->
  string ->
  color ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int * tile_type

val make_contents :
  string ->
  color ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  tile_type

val tiles_list : Yojson.Basic.t -> (int * tile_type) list
(** [tiles_list json] takes json representations of a monopoly board and returns
    a list of locations where each tile is associated with its index*)

val find_tile : int -> (int * tile_type) list -> tile_type
(** [find_tile index tiles] looks within the list of tiles to find the tile
    associated with [index]*)
