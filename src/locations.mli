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

type color

val tile_type : string -> Yojson.Basic.t -> tile_type
val tile_color : string -> color
val tile_index : int -> Yojson.Basic.t -> int * tile_type
val property_name : _property -> string
val tax_name : _tax -> string

val tiles_list : Yojson.Basic.t -> (int * tile_type) list
(** [tiles_list json] takes json representations of a monopoly board and returns
    a list of locations where each tile is associated with its index*)

val find_tile : int -> (int * tile_type) list -> tile_type
(** [find_tile index tiles] looks within the list of tiles to find the tile
    associated with [index]*)

val get_tile_name : int -> (int * tile_type) list -> string
(** [get_tile_name i mlist] returns the tile name with index [i] from the list
    [mlist]*)

(*val cc_chance : card -> unit*)

(* Helper Functions for tests*)
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
