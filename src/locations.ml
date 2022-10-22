open Yojson.Basic.Util

type _property = {
  name : string;
  price : int;
}

type tile_type =
  | Property of _property
  | Go
  | CommunityChest
  | Chance
  | Jail

(***************************************************************************
  Helper Functions
  *************************************************************************)
let to_json json = Yojson.Basic.from_file json
let loc_contents j = j |> member "contents"
let loc_type j = j |> member "tile type" |> to_string
let make_contents name price = Property { name; price }

let property contents =
  {
    name = contents |> member "tile name" |> to_string;
    price = contents |> member "price" |> to_string |> int_of_string;
  }

let tile_type tt con =
  match tt with
  | "property" -> Property (property con)
  | "go" -> Go
  | "cc" -> CommunityChest
  | "chance" -> Chance
  | "jail" -> Jail
  | _ -> assert false

let make_tile (index : int) type_of_tile nm prc =
  match type_of_tile with
  | "property" -> (index, Property { name = nm; price = prc })
  | "go" -> (index, Go)
  | "cc" -> (index, CommunityChest)
  | "chance" -> (index, Chance)
  | "jail" -> (index, Jail)
  | _ -> assert false

(******************************************************************************
  End of Helper Functions
  *************************************************************************)
let tile_index i loc =
  let con = loc_contents loc in
  let tt = loc_type loc in
  let hold = tile_type tt con in
  (i, hold)

let tl_helper a =
  let index, loc = a in
  let i = int_of_string index in
  tile_index i loc

let tiles_list json = json |> to_assoc |> List.map tl_helper

let rec find_tile index tiles =
  match tiles with
  | (x, y) :: t -> if x = index then y else find_tile index t
  | [] -> raise Not_found
