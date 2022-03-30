open Card
open Misc_helpers

exception InvalidColor of string
exception OthersDead of string

type color = Red | Black | Orange | Blue | Green | Purple | Blank

type phase = Gain | Place | Attack | Move | Wait

type player = {
  color : color;
  country_names : string list;
  cards : card list;
  troop_queue : int;
  alive : bool
}


(** For printing players ****************************************************)
(******************************************************************************)

let string_of_color c = 
  match c with
  | Red -> "Red"
  | Black -> "Black"
  | Orange -> "Orange"
  | Blue -> "Blue"
  | Green -> "Green"
  | Purple -> "Purple"
  | Blank -> "Blank"

let string_of_player p = 
  let open Printf in
  sprintf("Player: %s [ \n  country_names: [%s] \n  cards: \n%s]") 
  (string_of_color p.color) 
  (match p.country_names with 
  | [] -> ""
  | h :: t -> List.fold_left (Printf.sprintf("%s; %s")) h t)
  (string_of_card_list p.cards "    ")

  
(** For checking validity of players list *************************************)
(******************************************************************************)

let rec check_players players = 
  match players with
  | [] -> false
  | h :: t -> if h.alive then true else check_players t

  
(** For retrieving players ****************************************************)
(******************************************************************************)

let rec get_player_by_color 
  (player_list : player list)
  (color : color)
  =
  match player_list with 
  | [] -> raise (InvalidColor (string_of_color color ^ " is not a valid color"))
  | h :: t -> 
    if h.color = color then
      h
    else get_player_by_color t color

let rec wrap_list_helper players curr_color acc = 
  match players with 
  | [] -> raise (InvalidColor 
      (string_of_color curr_color ^ " is not a color in player list"))
  | h :: t -> 
    if h.color = curr_color then
      t @ (List.rev acc)
    else wrap_list_helper t curr_color (h :: acc)

let wrap_list players curr_color = 
  wrap_list_helper players curr_color []

let rec first_alive_player (wrapped_players : player list) = 
  match wrapped_players with 
  | [] -> raise (OthersDead "Other players are already dead")
  | h :: t -> if h.alive then h else first_alive_player t

let next_player players curr_color =
  if curr_color = Blank then
    List.hd players
  else
    let next_players = wrap_list players curr_color
    in first_alive_player next_players


(** For updating a single player and returning the updated player *************)
(******************************************************************************)

let remove_country
  (country_name : string)
  (player : player)
  =
  {player with 
  country_names = remove_element player.country_names country_name;
  alive = if player.country_names = [] then false else true}

let add_country
  (country_name : string)
  (player : player)
  =
  {player with country_names = country_name :: player.country_names}

let add_troops
  (troops : int)
  (player : player)
  = 
  {player with troop_queue = player.troop_queue + troops}


(** For updating the entire player list ***************************************)
(******************************************************************************)

let rec update_players_with_player
  (player_list : player list)
  (changed_player : player)
  =
  match player_list with 
  | [] -> raise (InvalidColor 
    ("Changed player's color: " 
    ^ string_of_color changed_player.color 
    ^ " is not a valid color"))
  | h :: t -> 
    if h.color = changed_player.color then
      changed_player :: t
    else
      h :: update_players_with_player t changed_player

let update_players_with_country 
  (player_list : player list)
  (changed_country_name : string)  
  (old_owner : color)
  (changed_owner : color)
  =
  if old_owner = changed_owner then
    player_list
  else 
    let update1 = old_owner
    |> get_player_by_color player_list
    |> remove_country changed_country_name
    |> update_players_with_player player_list
    in
    changed_owner
    |> get_player_by_color update1
    |> add_country changed_country_name
    |> update_players_with_player update1

let player_death_by_color
  (player_list : player list)
  (color : color)
  =
  let dead_player = get_player_by_color player_list color
    in
    update_players_with_player 
      player_list 
      {dead_player with alive = false}
  
