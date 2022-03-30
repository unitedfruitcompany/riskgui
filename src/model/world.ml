open Country
open Continent
open Player
open Card


type world_info = {
  all_continents : continent list;
  edges : (string * string) list;
  all_countries : country_info list;
  players : player list;
  deck : card list;
  sets_traded_in : int;
  turn_color : color;
  turn : int
}

(** For retrieving info from the world ****************************************)
(******************************************************************************)

let get_current_player world =
  get_player_by_color world.players world.turn_color


(** For updating the world ****************************************************)
(******************************************************************************)

let update_world_set_count
  (sets_traded_in : int)
  (world : world_info)
  =
  {world with sets_traded_in = world.sets_traded_in + sets_traded_in}

let update_world_with_player
  (world : world_info)
  (changed_player : player)
  = 
  {world with
    players = update_players_with_player world.players changed_player}

let update_world_with_country 
  (world : world_info) 
  (changed_country : country_info)
  =
  let old_country_owner = 
    (country_by_name world.all_countries changed_country.name).state.owner
  in
  {world with 
    all_countries = 
      update_countries_with_country world.all_countries changed_country;
    players = 
      update_players_with_country 
        world.players 
        changed_country.name
        old_country_owner
        changed_country.state.owner}

let update_world_with_state
  (world : world_info)
  (name : string)
  (changed_state : state) 
  =
  let changed_country = 
    name
    |> country_by_name world.all_countries
    |> update_state_of_country changed_state
  in
  update_world_with_country world changed_country

let update_world_with_owner 
  (world : world_info)
  (name : string)
  (changed_owner : color)
  =
  let changed_country = 
    name
    |> country_by_name world.all_countries
    |> update_owner_of_country changed_owner
  in
  update_world_with_country world changed_country

let update_world_with_troops 
  (world : world_info)
  (name : string)
  (changed_troops : int)
  =
  let changed_country = 
    name
    |> country_by_name world.all_countries
    |> update_troops_of_country changed_troops
  in
  update_world_with_country world changed_country
