open Yojson.Basic.Util

type property = {
  name : string;
  price : int;
}

type tile_type =
  | Property of property
  | Go

(***************************************************************************
  Helper Functions
  *************************************************************************)
let to_json json = Yojson.Basic.from_file json
let loc_contents j = j |> member "contents"

let property_record contents =
  {
    name = contents |> member "title name" |> to_string;
    price = contents |> member "price" |> to_int;
  }

(******************************************************************************
  End of Helper Functions
  *************************************************************************)

let parse_tile_type tile =
  let tile_type = tile |> member "tile type" |> to_string in
  match tile_type with
  | "property" -> Property (property_record (loc_contents tile))
  | "go" -> Go
  | _ -> assert false

let rec parse_tiles (index : int) (json : Yojson.Basic.t)
    (tiles_map : (int * tile_type) list) =
  if index = 40 then tiles_map
  else
    let tile = json |> member (string_of_int index) in
    parse_tiles (index + 1) json ((index, parse_tile_type tile) :: tiles_map)

let tiles_list json = parse_tiles 0 json []

let rec find_tile index tiles =
  match tiles with
  | (x, y) :: t -> if x = index then y else find_tile index t
  | [] -> raise Not_found
