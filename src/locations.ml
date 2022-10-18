open Yojson.Basic.Util

type property = {
  name : string;
  price : int;
}

type tile_type =
  | Property of property
  | Go

let to_json json = Yojson.Basic.from_file json
let property_contents j = j |> member "contents"

let property_record lst =
  {
    name = lst |> member "title name" |> to_string;
    price = lst |> member "price" |> to_int;
  }

let rec find_tile (index : int) (tiles : (int * tile_type) list) =
  match tiles with
  | (x, y) :: t -> if x = index then y else find_tile index t
  | [] -> raise Not_found

let rec initialize_tiles index (json : Yojson.Basic.t)
    (tiles_map : (int * tile_type) list) =
  if index = 39 then tiles_map
  else
    let tile = json |> member (string_of_int index) in
    let tile_type = tile |> member "tile type" |> to_string in
    match tile_type with
    | "property" ->
        (index, Property (property_record (property_contents tile)))
        :: tiles_map
        |> initialize_tiles (index + 1) json
    | "go" -> (index, Go) :: tiles_map |> initialize_tiles (index + 1) json
    | _ -> assert false

let parse_board json = initialize_tiles 0 json []
