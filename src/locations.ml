open Yojson.Basic.Util
open Player

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
    data the correlates to a player purchasble location*)
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

let tax contents =
  {
    name = contents |> member "tile name" |> to_string;
    tax = contents |> member "tax" |> to_string |> int_of_string;
  }

let property_name (p : _property) = p.name
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
  | Go -> "go"
  | CommunityChest -> "Community Chest"
  | Chance -> "Chance"
  | Jail -> "Jail"
  | VisitingJail -> "Visiting Jail"
  | Parking -> "Parking"

(* TODO: for tiles Chance and Community Chest, figure out how the functionality
   and or data type interacts with Cards*)
let cc_chance c =
  match c with
  | Chance ->
      failwith "todo: draw random card from chance deck"
      (*need to create a list of cards that has them shuffled in random order*)
  | CommunityChest -> failwith "todo:draw random card from cc deck"
  | _ -> failwith "will never match against these"

(* TODO: function/s for tiles like Jail. Potentailly make use of Player
   functions to adjust things like player position*)
let jail_pos player =
  if player.free_jail then begin
    print_endline
      "You have a get out of jail free card. Would you like to use it? Y/N";
    let use_card = read_line () in
    if use_card = "Y" then () else ()
  end
  else ();
  { player with board_position = 10; in_jail = true }
(*When you get sent to Monopoly jail, your turn then ends. You have to wait
  until your next turn to use the Get Out of Jail Free card, pay the fine, or
  attempt to roll a double.
  https://www.monopolyland.com/get-out-of-jail-free-cards/*)

(*******************************************************************************
  ******************************************************************************
  Helper functions for Locations Tests
  ******************************************************************************
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

let make_tile (index : int) type_of_tile name color price upgrade_cost base_rent
    lvl1 lvl2 lvl3 lvl4 hotel tax =
  match type_of_tile with
  | "property" ->
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
  | "railroad" ->
      ( index,
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
          } )
  | "utility" ->
      ( index,
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
          } )
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
