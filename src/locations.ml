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

(*******************************************************************************
  Helper functions for Locations Tests
  *****************************************************************************)
let make_contents name color price upgrade_cost base_rent lvl1 lvl2 lvl3 lvl4
    hotel =
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
    }

let tax contents =
  {
    name = contents |> member "tile name" |> to_string;
    tax = contents |> member "tax" |> to_string |> int_of_string;
  }

let make_tile (index : int) type_of_tile name color price upgrade_cost base_rent
    lvl1 lvl2 lvl3 lvl4 hotel =
  match type_of_tile with
  | "property" | "railroad" | "utility" ->
      ( index,
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
          } )
  | "tax" -> (index, Tax { name; tax })
  | "go" -> (index, Go)
  | "cc" -> (index, CommunityChest)
  | "chance" -> (index, Chance)
  | "jail" -> (index, Jail)
  | "visiting_jail" -> (index, VisitingJail)
  | "parking" -> (index, Parking)
  | _ -> assert false

(*******************************************************************************
  End helper functions for Locations Tests
  *****************************************************************************)

(***************************************************************************
  Helper Functions for Locations
  *************************************************************************)
let to_json json = Yojson.Basic.from_file json
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
  }

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

let rec find_tile index tiles =
  match tiles with
  | (x, y) :: t -> if x = index then y else find_tile index t
  | [] -> raise Not_found
