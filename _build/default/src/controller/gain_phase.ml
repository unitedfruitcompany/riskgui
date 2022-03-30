open Player
open Continent
open Misc_helpers
open World
open Card


let rec calculate_continent_bonuses acc continents country_names   = 
  match continents with
  | [] -> acc
  | h :: t -> 
    if is_subset h.country_names country_names 
      then calculate_continent_bonuses (acc + h.bonus) t country_names  
    else calculate_continent_bonuses acc t country_names  

let calculate_troops_map 
  (continents : continent list)
  (player : player) 
  =
  List.length (player.country_names) / 3 + 
  calculate_continent_bonuses 0 continents player.country_names  

let calculate_troops_cards 
  (world : world_info) 
  (player : player) 
  (card_set : card list)
  =
  cards_bonus player.country_names world.sets_traded_in card_set

let gain_troops_map
  (world : world_info)
  (player : player) 
  : world_info
  =
  let base_gain = calculate_troops_map world.all_continents player in
  let changed_player = add_troops base_gain player in
  update_world_with_player world changed_player

let gain_troops_cards
  (world : world_info)
  (player : player) 
  (card_set: card list)
  : world_info
  =
  match card_set with
  | x -> let changed_player =
    add_troops 
    (cards_bonus player.country_names world.sets_traded_in x)
    player in 
    changed_player
    |> update_world_with_player world
    |> update_world_set_count 1
  
  
  
