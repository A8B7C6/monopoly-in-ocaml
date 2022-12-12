open Yojson.Basic.Util

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

type _property = {
  name : string;
  color : color;
  price : int;
  upgrade_cost : int;
  base_rent : int;
  _1rent : int;
  _2rent : int;
  _3rent : int;
  _4rent : int;
  hotel_rent : int;
  owner : string option;
}

type _tax = {
  name : string;
  tax : int;
}

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

(***************************************************************************
  Parsing Functions
  *************************************************************************)

let loc_contents j = j |> member "contents"
let loc_type j = j |> member "tile type" |> to_string

let tile_color tt =
  match tt with
  | "brown" -> Brown
  | "light blue" -> LightBlue
  | "pink" -> Pink
  | "orange" -> Orange
  | "red" -> Red
  | "yellow" -> Yellow
  | "green" -> Green
  | "dark blue" -> DarkBlue
  | "colorless" -> Colorless
  | _ -> failwith "unmatched color"

(** [property con] parses data that aligns with type [_property]. This means any
    data that correlates to a player-purchasable location*)
let property contents =
  {
    name = contents |> member "tile name" |> to_string;
    color = contents |> member "color" |> to_string |> tile_color;
    price = contents |> member "price" |> to_string |> int_of_string;
    upgrade_cost =
      contents |> member "upgrade cost" |> to_string |> int_of_string;
    base_rent = contents |> member "base rent" |> to_string |> int_of_string;
    _1rent = contents |> member "1 upgrade rent" |> to_string |> int_of_string;
    _2rent = contents |> member "2 upgrades rent" |> to_string |> int_of_string;
    _3rent = contents |> member "3 upgrades rent" |> to_string |> int_of_string;
    _4rent = contents |> member "4 upgrades rent" |> to_string |> int_of_string;
    hotel_rent = contents |> member "hotel rent" |> to_string |> int_of_string;
    owner = None;
  }

let tax contents =
  {
    name = contents |> member "tile name" |> to_string;
    tax = contents |> member "tax" |> to_string |> int_of_string;
  }

let property_name (p : _property) = p.name
let property_price (p : _property) = p.price
let property_upgrade_cost (p : _property) = p.upgrade_cost

let property_rent (p : _property) (upgrade_lvl : int) =
  if upgrade_lvl = 1 then p._1rent
  else if upgrade_lvl = 2 then p._2rent
  else if upgrade_lvl = 3 then p._3rent
  else if upgrade_lvl = 4 then p._4rent
  else if upgrade_lvl = 5 then p.hotel_rent
  else p.base_rent

let tax_name t = t.name

let tile_type tt con =
  match tt with
  | "property" -> Property (property con)
  | "railroad" -> Railroad (property con)
  | "utility" -> Utility (property con)
  | "tax" -> Tax (tax con)
  | "go" -> Go
  | "cc" -> CommunityChest
  | "chance" -> Chance
  | "jail" -> Jail
  | "visiting jail" -> VisitingJail
  | "parking" -> Parking
  | _ -> assert false
(******************************************************************************
  End of Parsing Functions
  ****************************************************************************)

(******************************************************************************
  Helper Functions for Locations
  ****************************************************************************)
let tile_index i loc =
  let con = loc_contents loc in
  let tt = loc_type loc in
  let hold = tile_type tt con in
  (i, hold)

let tl_helper a =
  let index, loc = a in
  let i = int_of_string index in
  tile_index i loc
(******************************************************************************
  End of Helper Functions for Locations
  *************************************************************************)

let tiles_list json = json |> to_assoc |> List.map tl_helper
let monopoly_list = tiles_list (Yojson.Basic.from_file "data/Monopoly.json")

let rec find_tile index tiles =
  match tiles with
  | (x, y) :: t -> if x = index then y else find_tile index t
  | [] -> raise Not_found

let get_tile_name index mlist =
  match find_tile index mlist with
  | Property p -> property_name p
  | Railroad r -> property_name r
  | Utility u -> property_name u
  | Tax t -> tax_name t
  | Go -> "Go"
  | CommunityChest -> "Community Chest"
  | Chance -> "Chance"
  | Jail -> "Jail"
  | VisitingJail -> "Visiting Jail"
  | Parking -> "Parking"

(*******************************************************************************
  ******************************************************************************
                Helper functions for Locations Tests
 ******************************************************************************
*****************************************************************************)
let make_contents type_of_tile name color price upgrade_cost base_rent lvl1 lvl2
    lvl3 lvl4 hotel tax =
  match type_of_tile with
  | "property" ->
      Property
        {
          name;
          color;
          price;
          upgrade_cost;
          base_rent;
          _1rent = lvl1;
          _2rent = lvl2;
          _3rent = lvl3;
          _4rent = lvl4;
          hotel_rent = hotel;
          owner = None;
        }
  | "railroad" ->
      Railroad
        {
          name;
          color;
          price;
          upgrade_cost;
          base_rent;
          _1rent = lvl1;
          _2rent = lvl2;
          _3rent = lvl3;
          _4rent = lvl4;
          hotel_rent = hotel;
          owner = None;
        }
  | "utility" ->
      Utility
        {
          name;
          color;
          price;
          upgrade_cost;
          base_rent;
          _1rent = lvl1;
          _2rent = lvl2;
          _3rent = lvl3;
          _4rent = lvl4;
          hotel_rent = hotel;
          owner = None;
        }
  | "tax" -> Tax { name; tax }
  | _ -> assert false

let make_tile (index : int) type_of_tile name color price upgrade_cost base_rent
    lvl1 lvl2 lvl3 lvl4 hotel tax =
  match type_of_tile with
  | "property" ->
      ( index,
        make_contents type_of_tile name color price upgrade_cost base_rent lvl1
          lvl2 lvl3 lvl4 hotel tax )
  | "railroad" ->
      ( index,
        make_contents type_of_tile name color price upgrade_cost base_rent lvl1
          lvl2 lvl3 lvl4 hotel tax )
  | "utility" ->
      ( index,
        make_contents type_of_tile name color price upgrade_cost base_rent lvl1
          lvl2 lvl3 lvl4 hotel tax )
  | "tax" -> (index, Tax { name; tax })
  | "go" -> (index, Go)
  | "cc" -> (index, CommunityChest)
  | "chance" -> (index, Chance)
  | "jail" -> (index, Jail)
  | "visiting jail" -> (index, VisitingJail)
  | "parking" -> (index, Parking)
  | _ -> assert false

(*******************************************************************************
  End helper functions for Locations Tests
  *****************************************************************************)
