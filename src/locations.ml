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

let parse_tile_type tile = raise (Failure "unimplemented parse_tile_type")
let tiles_list json = raise (Failure "unimplemented tiles_list")

let rec find_tile index tiles =
  match tiles with
  | (x, y) :: t -> if x = index then y else find_tile index t
  | [] -> raise Not_found

(* let rec initialize_tiles index (json : Yojson.Basic.t) (tiles_map : (int *
   tile_type) list) = if index = 39 then tiles_map else let tile = json |>
   member (string_of_int index) in let tile_type = tile |> member "tile type" |>
   to_string in match tile_type with | "property" -> (index, Property
   (property_record (property_contents tile))) :: tiles_map |> initialize_tiles
   (index + 1) json | "go" -> (index, Go) :: tiles_map |> initialize_tiles
   (index + 1) json | _ -> assert false

   let parse_board json = initialize_tiles 0 json [] *)
