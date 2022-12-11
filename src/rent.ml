open Locations
open Player

type rent_tile = {
  player : _player option;
  tile : tile_type;
  mutable upgrade_level : int;
  mutable rent_cost : int;
}

let tiles = monopoly_list

let rents_map =
  Array.make 40
    {
      player = None;
      tile = snd (List.hd tiles);
      upgrade_level = 0;
      rent_cost = 0;
    }

let determine_rent_price tile_pos (t : tile_type) : int =
  let lvl = rents_map.(tile_pos).upgrade_level in
  match t with
  | Property p -> property_rent p lvl
  | _ -> 0

let rec create_properties_map t =
  match t with
  | [] -> ()
  | (x, y) :: t ->
      rents_map.(x) <-
        {
          player = None;
          tile = y;
          upgrade_level = 0;
          rent_cost = determine_rent_price x y;
        };
      create_properties_map t

let determine_rent board_pos =
  let rTile = rents_map.(board_pos) in
  match rTile.player with
  | None -> (0, None)
  | Some player -> (rTile.rent_cost, Some player)

let pay_rent player board_pos =
  let potential_owner = determine_rent board_pos in
  match potential_owner with
  | _, None -> ()
  | x, Some owner ->
      decrement_balance player x;
      distribute_change owner x;
      print_endline
        ("Player " ^ player.name ^ "paid " ^ owner.name ^ "$" ^ string_of_int x
       ^ " in rent.")

let is_purchasable (board_pos : int) =
  let rTile = rents_map.(board_pos) in
  match rTile.player with
  | None -> true
  | Some _ -> false

let purchase_property player price board_pos (rTile : rent_tile) : unit =
  if player.balance.total < price then
    print_endline
      "Sorry, you do not have enough balance to purchase this property"
  else
    rents_map.(board_pos) <-
      {
        rent_cost = rTile.rent_cost;
        tile = rTile.tile;
        player = Some player;
        upgrade_level = 0;
      };
  decrement_balance player price

let rec upgrade_property board_pos p price player =
  let _ =
    print_endline
      ("Would you like to upgrade " ^ property_name p ^ " for "
     ^ string_of_int price ^ ": y/n")
  in
  let upgrade_response = read_line () in
  match upgrade_response with
  | "y" ->
      if player.balance.total < price then
        print_endline
          "Sorry, you do not have enough balance to purchase this property"
      else decrement_balance player price;
      rents_map.(board_pos).upgrade_level <-
        rents_map.(board_pos).upgrade_level + 1;
      rents_map.(board_pos).rent_cost <-
        determine_rent_price board_pos rents_map.(board_pos).tile
  | "n" -> ()
  | "" | _ ->
      print_endline "Please provide a valid response: y/n";
      upgrade_property board_pos p price player

let rec check_property board_pos player =
  let rTile = rents_map.(board_pos) in
  match rTile.tile with
  | Property p ->
      if is_purchasable board_pos then (
        let _ =
          print_endline
            ("Would you like to purchase " ^ property_name p ^ " for "
            ^ string_of_int (property_price p)
            ^ ": y/n")
        in
        let buy_response = read_line () in
        match buy_response with
        | "y" -> purchase_property player (property_price p) board_pos rTile
        | "n" -> ()
        | "" | _ ->
            print_endline "Please provide a valid response: y/n";
            check_property board_pos player)
      else if rTile.rent_cost != 0 then
        match rTile.player with
        | Some pl ->
            let price = property_upgrade_cost p in
            if pl.name = player.name then
              upgrade_property board_pos p price player
            else pay_rent pl board_pos
        | _ -> ()
      else ()
  | _ -> ()

let _ = create_properties_map tiles
