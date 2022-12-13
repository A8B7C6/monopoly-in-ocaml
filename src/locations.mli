(** Representation of Static Locations on a Monopoly Board

    The module represents the information stored in data/Monopoly.json makes
    "tiles" out of different locations on a Monopoly Board. Handles majority of
    loading and querying the data. Rent Module handles renting functionality for
    some of the tiles*)

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

val tile_color : string -> color
(** [tile_color c] generates a [color] based on the string [c]*)

val property_name : _property -> string
(** [property_name t] grabes the name of a tile [t] of [tile_type] [Property],
    [Railroad], or [Utility]*)

val property_price : _property -> int
(** [property_price t] grabes the price to purchase a tile [t] of [tile_type]
    [Property], [Railroad], or [Utility]*)

val property_upgrade_cost : _property -> int
(** [property_upgrade_cost t] grabes the price to upgrade a tile [t] of
    [tile_type] [Property], [Railroad], or [Utility]*)

val tax_name : _tax -> string
(**[tax_name tax] returns the name of [tax]*)

val tile_type : string -> Yojson.Basic.t -> tile_type
(** [tile_type tt con] returns a tile of the type [tt] with the information from
    [con]*)

val tiles_list : Yojson.Basic.t -> (int * tile_type) list
(** [tiles_list json] takes json representations of a monopoly board and returns
    a list of locations where each tile is associated with its index*)

val find_tile : int -> (int * tile_type) list -> tile_type
(** [find_tile index tiles] looks within the list of tiles to find the tile
    associated with [index]*)

val get_tile_name : int -> (int * tile_type) list -> string
(** [get_tile_name i mlist] returns the tile name with index [i] from the list
    [mlist]*)

val monopoly_list : (int * tile_type) list
(** [monopoly_list] is an instance that holds data from Monopoly.json as
    [(k,v) list]. [k] represents the position of a tile on a board as an int.
    [v] holds the information about each tile*)

val property_rent : _property -> int -> int
(** [property_rent property ul] returns the cost of rent for [property] at the
    level [ul]*)

val make_contents :
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
  tile_type
(** [make_contents type_of_tile name color price upgrade_cost base_rent lvl1 lvl2
    lvl3 lvl4 hotel tax]
    makes a tile of type [Property], [Utility], [Railroad], or makes [Tax]. Used
    exclusively for testing.*)

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
(** [make_tile type_of_tile name color price upgrade_cost base_rent
    lvl1 lvl2 lvl3 lvl4 hotel tax]
    creates a tile of type [type_of_tyle] and associates it with the proper
    index. Used exclusively for testing *)
